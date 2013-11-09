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


(define log-file (let ((p (command-line
                            #:args (filename)
                            filename)))
                   (if (absolute-path? p)
                     p
                     (build-path (current-directory) p))))

; TODO: add auto fetch on scrolling (faster)
; TODO: add a checkbox to enable RAW format

(module+ test
         (require rackunit))

; weechat regex
(define re-date #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})")
(define re-saying #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t@?([^\\* -<][^\t]+)\t(.*)")
(define re-me #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t \\*\t(.*)")
(define re-action #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t(<--|--|-->)\t(.*)")

(module+ test
         (check-equal? 4 (length (regexp-match re-saying "2013-10-20 18:03:32\tfridim\ttu quoque mi fili")))
         (check-false (regexp-match? re-action "2013-10-20 18:03:32\tfridim\ttu quoque mi fili"))
         (check-true (regexp-match? re-action "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
         (check-true (regexp-match? re-me "2013-10-25 23:28:01\t *\tfridim sifflote"))
         (check-false (regexp-match? re-saying "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
         (check-true (regexp-match? re-action "2013-10-21 17:26:06\t-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil"))
         (check-true (regexp-match? re-action "2013-10-23 14:35:10\t--\tMode #esil [+o meow`] par ChanServ")))

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
    (let ((from (seconds->date from-s))
          (to (seconds->date to-s)))
      `(table
         ,@(let ((in (open-input-file log-file)))
             (for*/list ((line (in-lines in))
                         (r-saying (in-value (regexp-match re-saying line)))
                         (r-action (in-value (regexp-match re-action line)))
                         (r-me (in-value (regexp-match re-me line)))
                         #:when (and (string>=? line (date->string from))
                                     (or r-saying r-action r-me)
                                     (match? line)))
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
  (make-cdata #f #f (include-template "templates/index.html")))

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
