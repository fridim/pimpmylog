#lang rackjure

(require web-server/servlet-env
         web-server/templates
         web-server/dispatch
         web-server/servlet
         racket/cmdline
         racket/string
         racket/list
         (only-in racket/date date->seconds)
         (only-in srfi/19 date->string)
         xml
         "message.rkt"
         "formats/weechat.rkt"
         "formats/racket-org.rkt")

(define weechat-mode (make-parameter #t))
(define racket-lang-org-mode (make-parameter #f))
(define chan-name (make-parameter ""))
(define port (make-parameter 8000))

(module+ main
         (define log-file
           (let ((p (command-line
                      #:program "pimpmylog"
                      #:once-any
                      [("-w" "--weechat")
                       "input file is in weechat log format"
                       (weechat-mode #t)]
                      [("-r" "--racket-org")
                       "input file is in format as files on http://racket-lang.org/irc-logs/racket/"
                       (racket-lang-org-mode #t)]
                      #:once-each
                      [("-n" "--name") cn
                                       "Name of the channel"
                                       (chan-name cn)]
                      [("-p" "--port") p
                                       "Listening port"
                                       (port (string->number p))]
                      #:args (filename)
                      filename)))
             (if (absolute-path? p)
               p
               (build-path (current-directory) p)))))

(module+ main
         (define string->message
           (cond ((racket-lang-org-mode)
                  racket-org-string->message)
                 (else
                   weechat-string->message)))
         (define string->date
           (cond ((racket-lang-org-mode)
                  racket-org-string->date)
                 (else
                   weechat-string->date))))

(define (highlight msg search-str)
  (if search-str
    (regexp-replace* (str "((?i:" (regexp-quote search-str) "))")
                     msg
                     (str "<span class=\"highlight\">\\1</span>"))
    msg))

(define (htmlize msg [search-str #f])
  (define (transform-url url)
    (let ((url-clean (regexp-replace* #px"<span class=\"highlight\">([^<]+)</span>" url "\\1")))
      (str "<a href=\"" url-clean "\">" url "</a>")))

  (let ((escaped-msg (xexpr->string `,msg)))
    (regexp-replace* #px"https?://(?:<span |[^ ])+"
                     (if search-str
                       (highlight escaped-msg (xexpr->string `,search-str))
                       escaped-msg)
                     transform-url)))

(module+ test
         (require rackunit)

         (let ((foo "Lorem Ipsum is simply dummy text of the printing and typesetting industry.")
               (foo-h "Lorem Ipsum is simpl<span class=\"highlight\">y</span> dumm<span class=\"highlight\">y</span> text of the printing and t<span class=\"highlight\">y</span>pesetting industr<span class=\"highlight\">y</span>."))
           (check-equal? foo-h (highlight foo "y"))
           (check-equal? foo (highlight foo #f)))

         (check-equal? (htmlize "<h1>bla</h1>")
                       "&lt;h1&gt;bla&lt;/h1&gt;")
         (check-equal? (htmlize "<h1>bla</h1> foo<" "fOo<")
                       "&lt;h1&gt;bla&lt;/h1&gt; <span class=\"highlight\">foo&lt;</span>")
         (check-equal? (htmlize "<h1>bla</h1> fOo<" "foo<")
                       "&lt;h1&gt;bla&lt;/h1&gt; <span class=\"highlight\">fOo&lt;</span>")
         (check-equal? (htmlize "foo bar http://foobar.com?a&bla" "foo")
                       "<span class=\"highlight\">foo</span> bar <a href=\"http://foobar.com?a&amp;bla\">http://<span class=\"highlight\">foo</span>bar.com?a&amp;bla</a>")
         (check-equal?  (htmlize "http://onfi.re?a=2&bla .")
                        "<a href=\"http://onfi.re?a=2&amp;bla\">http://onfi.re?a=2&amp;bla</a> .")
         (check-equal? (htmlize "http://onfi.re .")
                       "<a href=\"http://onfi.re\">http://onfi.re</a> .")
         (check-equal?  (htmlize "http://onfi.re http://google.com .")
                        "<a href=\"http://onfi.re\">http://onfi.re</a> <a href=\"http://google.com\">http://google.com</a> ."))

(define colors (list "#000000" ; black
                     "#004386" ; blue
                     "#4E93D8" ; light blue
                     "#7C0900" ; red
                     "#534400" ; brown
                     "#2C5E2E" ; green
                     "#7E0091" ; purple
                     "#A95F04" ; orange
                     "#A9049F" ; pink
                     "#006A5D" ; blue-green
                     "#0C85FF" ; blue
                     "#00C026" ; green
                     "#8CCD00"
                     "#7C00CD"))

(define (nickname->color nickname)
  (let ((hashkey (modulo (+ (string-length nickname)
                            (apply + (map char->integer
                                          (string->list nickname))))
                         (length colors))))
    (list-ref colors hashkey)))

(define (id-date date)
  (~> date
      (string-replace " " "")
      (string-replace ":" "")))

(define (message->row m search-str)
  (define date (message-date m))
  (define nick (message-nick m))
  (define type (message-type m))
  (define msg (message-msg m))
  (define d (id-date date))
  (cond ((eq? type 'saying)
         `(tr ([class "saying infinite-item"] [id ,d])
              (td ([class "date"] )
                  (a ([id ,(str d "a")]
                      [class "anchor"]))
                  (a ([href ,(str "#" d "a")]) ,date))
              (td ([class "nick"]
                   [style ,(str "color: " (nickname->color nick))])
                  ,nick)
              (td ([class "msg"])
                  ,(make-cdata #f #f (htmlize msg search-str)))))
        ((eq? type 'me)
         `(tr ([class "me infinite-item"] [id ,d])
              (td ([class "date"])
                  (a ([id ,(str d "a")]
                      [class "anchor"]))
                  (a ([href ,(str "#" d "a")]) ,date))
              (td ([class "nick"]) "*")
              (td ([class "msg"]) ,(make-cdata #f #f
                                               (htmlize
                                                 (str nick " " msg)
                                                 search-str)))))
        ((or (eq? type 'action)
             (eq? type 'info ))
         `(tr ([class "action infinite-item"] [id ,d])
              (td ([class "date"])
                  (a ([id ,(str d "a")]
                      [class "anchor"]))
                  (a ([href ,(str "#" d "a")]) ,date))
              (td ([class "msg"] [colspan "2"]) ,msg)))))

(module+ main
         (define oldest
           (for*/first ((line (in-lines (open-input-file log-file)))
                        (d (in-value (string->date line))))
                       (date->seconds d)))

         (define (show-logs req howmuch [search-str #f] [from #f])
           (define (match? line)
             (if search-str
               (regexp-match?
                 (str "(?i:" (regexp-quote search-str) ")")
                 line)
               #t))

           (define (get-logs from-s [infinite? #f])
             (define to-s (+ (max oldest from-s) (* 3600 24)))
             (define cache
               (cond
                 ((< to-s (- (current-seconds) 60))
                  ; if it's in the past, cache indefinitly
                  "31536000")
                 ((and (eq? "all" howmuch) search-str)
                  ; if it's a search on all, let's say... one day
                  (* 3600 24))
                 (else "60"))) ; 1 min
             (define from (date->string (seconds->date from-s)
                                        "~Y-~m-~d ~H:~M:~S"))
             (define to (date->string (seconds->date to-s)
                                      "~Y-~m-~d ~H:~M:~S"))
             (define in (open-input-file log-file))
             (response/xexpr
               #:code 200
               #:headers (list (make-header
                                 #"Cache-Control"
                                 (string->bytes/utf-8
                                   (format "max-age=~a" cache))))
               `(table
                  ([id "logs"])
                  (tr ([id "firstline"]))
                  ,@(for/list
                      ((line (in-lines in))
                       #:when (and (match? line) (string>=? line from))
                       #:break (and (not search-str) (string>? line to)))
                      (message->row (string->message line) search-str))
                  ,(if infinite?
                     `(tr (td (a ([class "infinite-more-link"]
                                  [href ,(str "search?from="
                                              (number->string to-s))])
                                 "")))
                     `(tr ([id "lastline"]))))))

           (case howmuch
             (("all") (get-logs 1 #t))
             (("year") (get-logs (- (current-seconds) (* 3600 24 365)) #t))
             (("month") (get-logs (- (current-seconds) (* 3600 24 31)) #t))
             (("week") (get-logs (- (current-seconds) (* 3600 24 7)) #t))
             (("day") (get-logs (- (current-seconds) (* 3600 24))))
             (("timestamp")
              (cond ((< from (current-seconds))
                     (get-logs from #t))
                    ((>= from (current-seconds))
                     (response/xexpr `(p "(current-seconds)")))
                    (else
                      (response/xexpr
                        `(div (h1 "ERROR") (p "Maybe you should go."))))))))

         (define (search-logs req)
           (define bindings (request-bindings req))
           (cond ((exists-binding? 'q bindings)
                  (define q (extract-binding/single 'q bindings))
                  (if (= (string-length q) 0)
                    (response/xexpr `(p "empty string mate!"))
                    (show-logs req "all" q)))
                 ((exists-binding? 'from bindings)
                  (define from (string->number
                                 (extract-binding/single 'from bindings)))
                  (show-logs req "timestamp" #f from))
                 ((exists-binding? 'from-literal bindings)
                  (define from
                    (~> (extract-binding/single 'from-literal bindings)
                        (string->date "~Y-~m-~d")
                        date->seconds))
                  (show-logs req "timestamp" #f from))))

         (define-values (site-dispatch site-url)
           (dispatch-rules
             [("") index]
             [("logs" (string-arg)) show-logs]
             [("search") search-logs]))

         (define (start request)
           (site-dispatch request)))


(define (index req)
  (define channel (chan-name))
  ; TODO: don't use xexpr here...
  (response/xexpr (make-cdata #f #f (include-template "templates/index.html"))))

(module+ main
         (serve/servlet start
                        #:servlet-path "/"
                        #:servlet-regexp #rx""
                        #:log-file "server.log"
                        #:listen-ip "0.0.0.0"
                        #:port (port)
                        #:launch-browser? #f
                        #:extra-files-paths
                        (list
                          (build-path "htdocs"))))
