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

(module+ main
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
               (build-path (current-directory) p)))))

; TODO: add listening port as option
; TODO: log format definitions should be in a separate forlder
; TODO: add ajax fetch on scrolling (faster)
; TODO: add a checkbox to enable RAW format
; TODO: add color for nicknames
; IDEA: support multiple files ?


(struct message (date type nick msg) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Weechat log format
;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->message re type)
  (let ([date (cadr re)]
        [nick (caddr re)]
        [msg (cadddr re)])
    (message date type nick msg)))

(define weechat-re-date   #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})")
(define weechat-re-saying #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t@?([^ -<][^\t]+)\t(.*)")
(define weechat-re-me     #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t \\*\t([^ ]+) (.*)")
(define weechat-re-action #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t(<--|-->)\t([^ ]+) (.*)")
(define weechat-re-info   #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t--()\t(.*)")

(define (weechat-string->message line)
  (let ((r-saying (regexp-match weechat-re-saying line)))
    (if r-saying
      (list->message r-saying 'saying)
      (let ((r-action (regexp-match weechat-re-action line)))
        (if r-action
          (let* ([date (cadr r-action)]
                 [nick (cadddr r-action)]
                 [msg (string-append (caddr r-action) " " nick " " (last r-action))])
            (message date 'action nick msg))
          (let ((r-me (regexp-match weechat-re-me line)))
            (if r-me
              (list->message r-me 'me)
              (let ((r-info (regexp-match weechat-re-info line)))
                (if r-info
                  (list->message r-info 'info)
                  (raise (string-append "no match for line: " line)))))))))))

(module+ test
         (require rackunit)
         (check-equal? (weechat-string->message "2013-10-20 18:03:32\tfridim\ttu quoque mi fili")
                       (message "2013-10-20 18:03:32" 'saying "fridim" "tu quoque mi fili"))
         (check-equal? (weechat-string->message "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)")
                       (message "2013-10-20 18:32:05" 'action "v0n" "<-- v0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
         (check-equal? (weechat-string->message "2013-10-25 23:28:01\t *\tfridim sifflote")
                       (message "2013-10-25 23:28:01" 'me "fridim" "sifflote"))
         (check-equal? (weechat-string->message "2013-10-23 14:35:10\t--\tMode #esil [+o meow`] par ChanServ")
                       (message "2013-10-23 14:35:10" 'info "" "Mode #esil [+o meow`] par ChanServ"))
         (check-equal? (weechat-string->message "2013-10-21 17:26:06\t-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil")
                       (message "2013-10-21 17:26:06" 'action "v0n" "--> v0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil"))
         (check-equal? (message-type
                         (weechat-string->message "2013-11-01 20:03:46	<--	add^_ (~user@m5-241-189-182.cust.tele2.se) a quitté (Quit: ERC Version 5.3 (IRC client for Emacs))"))
                       'action)
         (check-equal? (weechat-string->message "2013-11-01 20:06:25	-->	gcartier (~gcartier@modemcable010.136-201-24.mc.videotron.ca) a rejoint #scheme")
                       (message "2013-11-01 20:06:25" 'action "gcartier" "--> gcartier (~gcartier@modemcable010.136-201-24.mc.videotron.ca) a rejoint #scheme"))
         (check-equal? (weechat-string->message "2013-11-01 20:06:51	--	AlterSid est maintenant connu sous le nom Guest99037")
                       (message "2013-11-01 20:06:51" 'info "" "AlterSid est maintenant connu sous le nom Guest99037"))
         (check-equal? (weechat-string->message "2013-11-01 20:24:25	LeoNerd	(EVERYBODY LOVES SHOUTY 'LISP)")
                       (message "2013-11-01 20:24:25" 'saying "LeoNerd" "(EVERYBODY LOVES SHOUTY 'LISP)"))
         (check-equal? (weechat-string->message "2013-10-30 16:28:15	--	v0n est maintenant connu sous le nom v0n|w")
                       (message "2013-10-30 16:28:15" 'info "" "v0n est maintenant connu sous le nom v0n|w")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket-org log format
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define racket-org-re-date   #px"^(\\d{4}-\\d{2}-\\d{2} \\d\\d:\\d{2})")
(define racket-org-re-saying #px"^(\\d{4}-\\d{2}-\\d{2} \\d\\d:\\d{2}) ([^: ]+): (.*)")
(define racket-org-re-action #px"^(\\d{4}-\\d{2}-\\d{2} \\d\\d:\\d{2}) ((?:\\(join\\)|\\(part\\)|\\(notice\\)|\\(quit\\)|\\(nick\\)|\\(topic\\)|\\(names\\)) ([^: ]+).*)")
(define racket-org-re-me     #px"^(\\d{4}-\\d{2}-\\d{2} \\d\\d:\\d{2}) ([^ ]+) (.*)")

(define (racket-org-string->message line)
  (let ((r-saying (regexp-match racket-org-re-saying line)))
    (if r-saying
      (list->message r-saying 'saying)
      (let ((r-action (regexp-match racket-org-re-action line)))
        (if r-action
          (let ([date (cadr r-action)]
                [nick (cadddr r-action)]
                [msg (caddr r-action)])
            (message date 'action nick msg))
          (let ((r-me (regexp-match racket-org-re-me line)))
            (if r-me
              (list->message r-me 'me)
              (raise (string-append "no match for line: " line)))))))))

(module+ test
         (check-true (regexp-match? racket-org-re-saying "2010-09-06 21:45 black_13: different paradigm"))
         (check-true (regexp-match? racket-org-re-action "2010-09-06 11:47 (join) adadglgmut"))
         (check-true (regexp-match? racket-org-re-me "2010-10-14 07:00 Lajla hides"))
         (check-equal? (racket-org-string->message "2011-01-12 21:37 _p4bl0: bremner: geiser rocks")
                       (message "2011-01-12 21:37" 'saying "_p4bl0" "bremner: geiser rocks"))
         (check-equal? (racket-org-string->message "2012-05-10 00:20 (quit) kreol[Ukr]: Ping timeout: 252 seconds")
                       (message "2012-05-10 00:20" 'action "kreol[Ukr]" "(quit) kreol[Ukr]: Ping timeout: 252 seconds"))
         (check-equal? (racket-org-string->message "2012-10-02 16:00 (part) _tca")
                       (message "2012-10-02 16:00" 'action "_tca" "(part) _tca"))
         (check-equal? (racket-org-string->message "2012-10-02 16:00 ozzloy: oh hey, people are here")
                       (message "2012-10-02 16:00" 'saying "ozzloy" "oh hey, people are here"))
         (check-equal? (racket-org-string->message "2012-10-02 16:00 bremner hides")
                       (message "2012-10-02 16:00" 'me "bremner" "hides"))
         (check-equal? (racket-org-string->message "2012-10-02 16:01 ozzloy: regarding https://github.com/plt/racket/wiki/Racket2 i recall one feature discussed was using #:else instead of [else ...]")
                       (message "2012-10-02 16:01" 'saying "ozzloy" "regarding https://github.com/plt/racket/wiki/Racket2 i recall one feature discussed was using #:else instead of [else ...]"))
         (check-equal? (racket-org-string->message "2013-11-13 08:19 (quit) statonjr: Ping timeout: 246 seconds")
                       (message "2013-11-13 08:19" 'action "statonjr" "(quit) statonjr: Ping timeout: 246 seconds"))
         (check-equal? (racket-org-string->message "2013-11-13 08:19 (nick) statonjr_ -> statonjr")
                       (message "2013-11-13 08:19" 'action "statonjr_" "(nick) statonjr_ -> statonjr"))
         (check-equal? (racket-org-string->message "2013-11-13 08:24 (join) yacks")
                       (message "2013-11-13 08:24" 'action "yacks" "(join) yacks"))
         (check-equal? (racket-org-string->message "2013-10-31 00:09 (quit) rmathews: Quit: Bye..")
                       (message "2013-10-31 00:09" 'action "rmathews" "(quit) rmathews: Quit: Bye..")))

(module+ main
         (define string->message
           (cond ((racket-lang-org-mode)
                  racket-org-string->message)
                 (else
                   weechat-string->message))))

(define (htmlize msg)
  (define (transform-url url)
    (xexpr->string `(a ([href ,url]) ,url)))
  (regexp-replace* #px"https?://[^ ]+" msg transform-url))

(module+ test
         (check-equal? "<a href=\"http://fridim.org\">http://fridim.org</a> ."
                       (htmlize "http://fridim.org .")))

(date-display-format 'iso-8601)  ; %Y-%m-%d

(module+ main
         (define (show-logs req howmuch [search-str #f] [timestamp #f])

           (define (match? line)
             (if search-str
               (regexp-match? search-str line)
               #t))

           (define (id-date date)
             (string-replace
               (string-replace date " " "") ":" ""))

           (define (get-logs from-s [to-s (current-seconds)])
             (let ((cache
                     (cond
                       ((< to-s (- (current-seconds) 60))
                        ; if it's in the past, cache indefinitly
                        "31536000")
                       ((and (eq? "all" howmuch) search-str)
                        ; if it's a search on all, let's say... one day
                        (* 3600 24))
                       (else
                         ; cache 1 hour for /logs/day
                         ; cache 7 hour for /logs/week
                         ; cache 31 hours for /logs/month
                         ; cache 15 days for /logs/year
                         (number->string (/ (- to-s from-s) 24))))))
               (response/xexpr
                 #:code 200
                 #:headers (list (make-header
                                   #"Cache-Control"
                                   (string->bytes/utf-8
                                     (format "max-age=~a" cache))))
                 (let ((from (date->string (seconds->date from-s)))
                       (to (string-append (date->string (seconds->date to-s)) " 23:59")))
                   `(table
                      ,@(let ((in (open-input-file log-file)))
                          (for*/list ((line (in-lines in))
                                      #:when (and (match? line)
                                                  (string>=? line from))
                                      #:break (string>? line to)
                                      (m (in-value (string->message line))))
                                     (let ([date (message-date m)]
                                           [nick (message-nick m)]
                                           [type (message-type m)]
                                           [msg (message-msg m)])
                                       (cond ((eq? type 'saying)
                                              `(tr ([class "saying"] [id ,(id-date date)])
                                                   (td ([class "date"] ) (a ([href ,(string-append "#" (id-date date))]) ,date))
                                                   (td ([class "nick"]) ,nick)
                                                   (td ([class "msg"]) ,(make-cdata #f #f (htmlize msg)))))
                                             ((eq? type 'me)
                                              `(tr ([class "me"] [id ,(id-date date)])
                                                   (td ([class "date"]) (a ([href ,(string-append "#" (id-date date))]) ,date))
                                                   (td ([class "nick"]) "*")
                                                   (td ([class "msg"]) ,(string-append nick " " msg))))
                                             ((or (eq? type 'action)
                                                  (eq? type 'info ))
                                              `(tr ([class "action"] [id ,(id-date date)])
                                                   (td ([class "date"]) (a ([href ,(string-append "#" (id-date date))]) ,date))
                                                   (td ([class "msg"] [colspan "2"]) ,msg))))))))))))

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
           (site-dispatch request)))


(define (index req)
  ; TODO: don't use xexpr here...
  (response/xexpr (make-cdata #f #f (let ((channel (chan-name)))
                                      (include-template "templates/index.html")))))

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
