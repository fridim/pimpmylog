#lang rackjure

(require web-server/servlet-env
         web-server/templates
         web-server/dispatch
         web-server/servlet
         racket/cmdline
         racket/string
         racket/list
         (only-in racket/date date->seconds)
         (only-in srfi/19 date->string string->date)
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

; produces a hash :
; * key : date by day
; * value : position in the log file to pass to file-position
(define (map-file f)
  (let-values ([(size resulting-hash last-day)
                (for*/fold ([bytes-count 0]
                            [h (hash)]
                            [previous-day #""])
                           ([l (in-bytes-lines (open-input-file f))]
                            [day (in-value (first (regexp-match*
                                                   ; TODO: use a regex per format (weechat, ...)
                                                   #px"^(\\d{4}-\\d{2}-\\d{2})"
                                                   l)))])
                  (values (+ 1 bytes-count (bytes-length l))
                          (if (bytes=? previous-day day)
                              h
                              (hash-set h day bytes-count))
                          day))])
    resulting-hash))

; in a list ls of possible values, find the nearest to d
(define (nearest ls d)
  (define (bytes->seconds b)
    (~> b
        bytes->string/utf-8
        (string->date "~Y-~m-~d")
        date->seconds))
  (define d-seconds (bytes->seconds d))
  (define (distance-from-d a)
    (abs (- (bytes->seconds a)
            d-seconds)))

  (define-values (minimum-date minimum-distance)
    (for*/fold ([current-min (first ls)]
                [current-min-distance (distance-from-d (first ls))])
               (; stop if the day-before or the day-after is found
                #:break (< current-min-distance 86401)
                [i (in-list ls)]
                [d-i (in-value (distance-from-d i))])
      (if (< current-min-distance d-i)
          (values current-min current-min-distance)
          (values i d-i))))
  minimum-date)

(define (find-nearest-key h d)
  ; TODO: idea for better performances : reduce the list of keys by looking
  ; only in current/previous/next months.
  (nearest (hash-keys h) d))

(module+ test
  (require rackunit)

  (check-equal?
   (nearest (list #"2014-09-10"
                  #"2013-08-02"
                  #"2011-01-01"
                  #"2010-12-24")
            #"2010-12-29")
   #"2011-01-01"))

; adjust file-position with a date
; - h: a hash to map dates to file-positions
; - in: the input-port
; - complete-date: a string of the date
(define (jump-to h in complete-date)
  (define key (string->bytes/utf-8 (substring complete-date 0 10)))
  (cond ((hash-has-key? h key)
         (file-position in (hash-ref h key)))
        (else
         (file-position in (hash-ref h (find-nearest-key h key))))))

; look for the next day from a given day and position. It returns 2 values:
; * the next day bytes-string
; * the position of the next day in the file
; if not found returns:
; * day
; * pos
(define (look-for-new-day path day pos)
  (define in (open-input-file path))
  (file-position in pos)

  (for*/fold ([last-day day]
              [bytes-count pos])
             ([l (in-bytes-lines in)]
              [n (in-value (first (regexp-match*
                                   ; TODO: use a regex per format (weechat, ...)
                                   #px"^(\\d{4}-\\d{2}-\\d{2})"
                                   l)))]
              #:final (not (bytes=? n day)))
    (values n
            (if (bytes=? n day)
                (if (eof-object? in)
                    pos
                    (+ 1 bytes-count (bytes-length l)))
                bytes-count))))

(module+ main
  (define log-file-map (map-file log-file))

  ; update log-file-map in a state-full manner
  ; ( still use immutable hash )
  (define (update-hash!)
    (define last-day (last (sort (hash-keys log-file-map) bytes<?)))
    (define last-day-pos (hash-ref log-file-map last-day))
    (define-values (key value) (look-for-new-day log-file last-day last-day-pos))
    (when key
      (unless (bytes=? key last-day)
        (set! log-file-map (hash-set log-file-map key value)))))

  (define (update-hash-if-needed path)
    (do ((evt (filesystem-change-evt path)
              (filesystem-change-evt path)))
        (#f)
      (sync evt)
      (sleep 2)
      (update-hash!)))
  (define worker (thread (lambda () (update-hash-if-needed log-file))))
  (displayln "updater started")

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

(module+ main
  (define (message->row m search-str)
    (define date (message-date m))
    (cond ((= 0 (string-length date)) ; ignore those lines
           "")
          (else
           (define nick (message-nick m))
           (define type (message-type m))
           (define msg (message-msg m))
           (define d (id-date date))
           (define ds (date->seconds (string->date date)))
           (define update-timeline (str "$('#timeline').val(" ds ")"))
           (define date-td
             `(td ([class "date"] )
               (a ([id ,(str d "a")]
                   [class "anchor"]))
               (a ([onClick ,update-timeline]
                   [href ,(str "#" d "a")]) ,date)))

           (cond ((eq? type 'saying)
                  `(tr ([class "saying infinite-item"]
                        [id ,d]
                        [onMouseOver ,update-timeline])
                    ,date-td
                    (td ([class "nick"]
                         [style ,(str "color: " (nickname->color nick))])
                        ,nick)
                    (td ([class "msg"])
                        ,(make-cdata #f #f (htmlize msg search-str)))))
                 ((eq? type 'me)
                  `(tr ([class "me infinite-item"]
                        [id ,d]
                        [onMouseOver ,update-timeline])
                    ,date-td
                    (td ([class "nick"]) "*")
                    (td ([class "msg"]) ,(make-cdata #f #f
                                                     (htmlize
                                                      (str nick " " msg)
                                                      search-str)))))
                 ((or (eq? type 'action)
                      (eq? type 'info ))
                  `(tr ([class "action infinite-item"]
                        [id ,d]
                        [onMouseOver ,update-timeline])
                    ,date-td
                    (td ([class "msg"] [colspan "2"]) ,msg)))))))

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
      (define format-t "~Y-~m-~d ~H:~M:~S")
      (define from (date->string (seconds->date from-s) format-t))
      (define to (date->string (seconds->date to-s) format-t))
      (define in (open-input-file log-file))
      (jump-to log-file-map in from)
      (response/xexpr
       #:code 200
       #:headers (list (make-header
                        #"Cache-Control"
                        (string->bytes/utf-8
                         (format "max-age=~a" cache))))
       `(table
         ([id "logs"])
         (tr ([id "firstline"] [class "infinite-item"])
             (td ([class "script"] [colspan "3"])
                 ,(make-cdata
                   #f #f (str "<script>$('#timeline').val("
                              (number->string from-s)
                              ");</script>"))))
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


(module+ main
  (define (index req)
    (define channel (chan-name))
    (define current-time (current-seconds))
                                        ; TODO: don't use xexpr here...
    (response/xexpr (make-cdata #f #f (include-template "templates/index.html")))))

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
