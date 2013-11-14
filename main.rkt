#lang racket/base

(require web-server/servlet-env
         web-server/templates
         web-server/dispatch
         web-server/servlet
         racket/promise
         racket/cmdline
         racket/string
         racket/list
         racket/date
         xml)

(define weechat-mode (make-parameter #t))
(define racket-lang-org-mode (make-parameter #f))
(define chan-name (make-parameter ""))

(define log-file
  (let ((p (command-line
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
             #:args (filename)
             filename)))
    (if (absolute-path? p)
      p
      (build-path (current-directory) p))))

; TODO: fix tests
; TODO: add structure for a msg
; TODO: a new log-format support should be as simple as a procedure string -> struct msg
; TODO: add auto fetch on scrolling (faster)
; TODO: add a checkbox to enable RAW format

; weechat regex
(define weechat-re-date   #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})")
(define weechat-re-saying #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t@?([^\\* -<][^\t]+)\t(.*)")
(define weechat-re-me     #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t \\*\t(.*)")
(define weechat-re-action #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t(<--|--|-->)\t(.*)")

(define racket-org-re-date   #px"^(\\d{4}-\\d{2}-\\d{2} \\d:\\d{2})")
(define racket-org-re-saying #px"^(\\d{4}-\\d{2}-\\d{2} \\d:\\d{2}) ([^: ]+): (.*)")
(define racket-org-re-me     #px"^(\\d{4}-\\d{2}-\\d{2} \\d:\\d{2}) ([^\\(: ]+) (.*)")
(define racket-org-re-action #px"^(\\d{4}-\\d{2}-\\d{2} \\d:\\d{2})( )(\\(join\\)|\\(quit\\)|\\(nick\\)|\\(topic\\)|\\(names\\)) (.*)")

(module+ test
         (require rackunit)
         (check-equal? 4 (length (regexp-match weechat-re-saying "2013-10-20 18:03:32\tfridim\ttu quoque mi fili")))
         (check-false (regexp-match? weechat-re-action "2013-10-20 18:03:32\tfridim\ttu quoque mi fili"))
         (check-true (regexp-match? weechat-re-action "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
         (check-true (regexp-match? weechat-re-me "2013-10-25 23:28:01\t *\tfridim sifflote"))
         (check-false (regexp-match? weechat-re-saying "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
         (check-true (regexp-match? weechat-re-action "2013-10-21 17:26:06\t-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil"))
         (check-true (regexp-match? weechat-re-action "2013-10-23 14:35:10\t--\tMode #esil [+o meow`] par ChanServ"))
         (check-true (regexp-match? racket-org-re-saying "2010-09-06 1:45 black_13: different paradigm"))
         (check-true (regexp-match? racket-org-re-action "2010-09-06 1:47 (join) adadglgmut"))
         (check-true (regexp-match? racket-org-re-me "2010-10-14 7:00 Lajla hides")))

(define re-date
  (cond ((racket-lang-org-mode)
         racket-org-re-date)
        (else
          weechat-re-date)))

(define re-saying
  (cond ((racket-lang-org-mode)
         racket-org-re-saying)
        (else
          weechat-re-saying)))

(define re-me
  (cond ((racket-lang-org-mode)
         racket-org-re-me)
        (else
          weechat-re-me)))

(define re-action
  (cond ((racket-lang-org-mode)
         racket-org-re-action)
        (else
          weechat-re-action)))

(define (htmlize msg)
  (define (transform-url url)
    (xexpr->string `(a ([href ,url]) ,url)))
  (regexp-replace* #px"https?://[^ ]+" msg transform-url))

(module+ test
         (check-equal? "<a href=\"http://fridim.org\">http://fridim.org</a> ."
                       (htmlize "http://fridim.org .")))

(date-display-format 'iso-8601)  ; %Y-%m-%d

(define (show-logs req howmuch [search-str #f] [timestamp #f])

  (define (match? line)
    (if search-str
      (regexp-match? search-str line)
      #t))

  (define (id-date date)
    (string-replace
      (string-replace date " " "") ":" ""))

  (define (get-logs from-s [to-s (current-seconds)])
    (let ((from (date->string (seconds->date from-s)))
          (to (date->string (seconds->date to-s))))
      `(table
         ,@(let ((in (open-input-file log-file)))
             (for*/list ((line (in-lines in))
                         #:when (and (match? line)
                                     (string>=? line from))
                         #:break (string>? line (string-append to " 23:59"))
                         (r-saying (in-value (regexp-match re-saying line)))
                         (r-action (in-value (regexp-match re-action line)))
                         (r-me (in-value (regexp-match re-me line)))
                         #:when (or r-saying r-action r-me))
                        (cond (r-saying
                                (let ([date (cadr r-saying)]
                                      [nick (caddr r-saying)]
                                      [msg (cadddr r-saying)])
                                  `(tr ([class "saying"] [id ,(id-date date)])
                                       (td ([class "date"] ) (a ([href ,(string-append "/#" (id-date date))]) ,date))
                                       (td ([class "nick"]) ,nick)
                                       (td ([class "msg"]) ,(make-cdata #f #f (htmlize msg))))))
                              (r-me
                                (let ([date (cadr r-me)]
                                      [msg (string-join (cddr r-me))])
                                  `(tr ([class "me"] [id ,(id-date date)])
                                       (td ([class "date"]) (a ([href ,(string-append "/#" (id-date date))]) ,date))
                                       (td ([class "msg"] [colspan "2"]) ,(string-append "* " msg)))))
                              (r-action
                                (let ([date (cadr r-action)]
                                      [msg (string-join (cddr r-action))])
                                  `(tr ([class "action"] [id ,(id-date date)])
                                       (td ([class "date"]) (a ([href ,(string-append "/#" (id-date date))]) ,date))
                                       (td ([class "msg"] [colspan "2"]) ,msg))))))))))

  (case howmuch
    (("all") (get-logs 1))
    (("year") (get-logs (- (current-seconds) (* 3600 24 365))))
    (("month") (get-logs (- (current-seconds) (* 3600 24 31))))
    (("week") (get-logs (- (current-seconds) (* 3600 24 7))))
    (("day") (get-logs (- (current-seconds) (* 3600 24))))
    (("timestamp") (get-logs timestamp (+ timestamp (* 3600 24))))))

(define (search-logs req)
  (let ((bindings (request-bindings req)))
    (cond ((exists-binding? 'q bindings)
           (let ((q (extract-binding/single 'q bindings)))
             (if (= (string-length q) 0)
               `(html (head) (body (p "chaine vide mec")))
               (show-logs req "all" q))))
          ((exists-binding? 't bindings)
           (let ((t (extract-binding/single 't bindings)))
             (if (= (string-length t) 0)
               `(html (head) (body (p "chaine vide mec")))
               (show-logs req "timestamp" #f (string->number t)))))
          (else
            `(html (head) (body (h1 "ERROR") (p "Maybe you should go.")))))))

(define-values (site-dispatch site-url)
  (dispatch-rules
    [("") index]
    [("logs" (string-arg)) show-logs]
    [("search") search-logs]))

(define (start request)
  (response/xexpr (site-dispatch request)))

(define (index req)
  (make-cdata #f #f (let ((channel (chan-name)))
                      (include-template "templates/index.html"))))

(module+ main
         (serve/servlet start
                        #:servlet-path "/"
                        #:servlet-regexp #rx""
                        #:log-file "server.log"
                        #:listen-ip "0.0.0.0"
                        #:launch-browser? #f
                        #:extra-files-paths
                        (list
                          (build-path "htdocs"))))
