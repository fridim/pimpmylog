#lang racket/base

(require "../message.rkt"
         racket/list)

(provide weechat-string->message
         message->weechat-string)

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
(define weechat-re-action #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t(<--|-->)\t(([^ ]+) .*)")
(define weechat-re-info   #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\t--()\t(.*)")

(define (weechat-string->message line)
  (let ((r-saying (regexp-match weechat-re-saying line)))
    (if r-saying
      (list->message r-saying 'saying)
      (let ((r-action (regexp-match weechat-re-action line)))
        (if r-action
          (let ([date (cadr r-action)]
                [nick (last r-action)]
                [msg (string-append
                        (caddr r-action) "\t"
                        (cadddr r-action))])
            (message date 'action nick msg))
          (let ((r-me (regexp-match weechat-re-me line)))
            (if r-me
              (list->message r-me 'me)
              (let ((r-info (regexp-match weechat-re-info line)))
                (if r-info
                  (list->message r-info 'info)
                  (raise (string-append "no match for line: " line)))))))))))

(define (message->weechat-string m)
  (string-append (message-date m) "\t"
                 (let ((mt (message-type m)))
                   (cond
                     ((eq? mt 'saying)
                      (string-append (message-nick m)
                                     "\t"
                                     (message-msg m)))
                     ((eq? mt 'action)
                      (string-append (message-msg m)))
                     ((eq? mt 'me)
                      (string-append " *\t"
                                     (message-nick m)
                                     " "
                                     (message-msg m)))
                     ((eq? mt 'info)
                      (string-append "--\t"
                                     (message-msg m)))))))

(module+ test
         (require rackunit)
         (check-equal? (weechat-string->message "2013-10-20 18:03:32\tfridim\ttu quoque mi fili")
                       (message "2013-10-20 18:03:32" 'saying "fridim" "tu quoque mi fili"))
         (check-equal? (weechat-string->message "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)")
                       (message "2013-10-20 18:32:05" 'action "v0n" "<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
         (check-equal? (weechat-string->message "2013-10-25 23:28:01\t *\tfridim sifflote")
                       (message "2013-10-25 23:28:01" 'me "fridim" "sifflote"))
         (check-equal? (weechat-string->message "2013-10-23 14:35:10\t--\tMode #esil [+o meow`] par ChanServ")
                       (message "2013-10-23 14:35:10" 'info "" "Mode #esil [+o meow`] par ChanServ"))
         (check-equal? (weechat-string->message "2013-10-21 17:26:06\t-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil")
                       (message "2013-10-21 17:26:06" 'action "v0n" "-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil"))
         (check-equal? (message-type
                         (weechat-string->message "2013-11-01 20:03:46	<--	add^_ (~user@m5-241-189-182.cust.tele2.se) a quitté (Quit: ERC Version 5.3 (IRC client for Emacs))"))
                       'action)
         (check-equal? (weechat-string->message "2013-11-01 20:06:25	-->	gcartier (~gcartier@modemcable010.136-201-24.mc.videotron.ca) a rejoint #scheme")
                       (message "2013-11-01 20:06:25" 'action "gcartier" "-->\tgcartier (~gcartier@modemcable010.136-201-24.mc.videotron.ca) a rejoint #scheme"))
         (check-equal? (weechat-string->message "2013-11-01 20:06:51	--	AlterSid est maintenant connu sous le nom Guest99037")
                       (message "2013-11-01 20:06:51" 'info "" "AlterSid est maintenant connu sous le nom Guest99037"))
         (check-equal? (weechat-string->message "2013-11-01 20:24:25	LeoNerd	(EVERYBODY LOVES SHOUTY 'LISP)")
                       (message "2013-11-01 20:24:25" 'saying "LeoNerd" "(EVERYBODY LOVES SHOUTY 'LISP)"))
         (check-equal? (weechat-string->message "2013-10-30 16:28:15	--	v0n est maintenant connu sous le nom v0n|w")
                       (message "2013-10-30 16:28:15" 'info "" "v0n est maintenant connu sous le nom v0n|w"))

         (check-equal? (message->weechat-string (message "2013-10-20 18:03:32" 'saying "fridim" "tu quoque mi fili"))
                       "2013-10-20 18:03:32\tfridim\ttu quoque mi fili")
         (check-equal? (message->weechat-string (message "2013-10-20 18:32:05" 'action "v0n" "<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)"))
                       "2013-10-20 18:32:05\t<--\tv0n (~vivien@96.127.192.36) a quitté (Ping timeout: 245 seconds)")
         (check-equal? (message->weechat-string (message "2013-10-25 23:28:01" 'me "fridim" "sifflote"))
                       "2013-10-25 23:28:01\t *\tfridim sifflote")
         (check-equal? (message->weechat-string (message "2013-10-23 14:35:10" 'info "" "Mode #esil [+o meow`] par ChanServ"))
                       "2013-10-23 14:35:10\t--\tMode #esil [+o meow`] par ChanServ")
         (check-equal? (message->weechat-string (message "2013-10-21 17:26:06" 'action "v0n" "-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil"))
                       "2013-10-21 17:26:06\t-->\tv0n (~vivien@mtl.savoirfairelinux.net) a rejoint #esil")
         (check-equal? (message->weechat-string (message "2013-11-01 20:06:25" 'action "gcartier" "-->\tgcartier (~gcartier@modemcable010.136-201-24.mc.videotron.ca) a rejoint #scheme"))
                       "2013-11-01 20:06:25	-->	gcartier (~gcartier@modemcable010.136-201-24.mc.videotron.ca) a rejoint #scheme")
         (check-equal? (message->weechat-string (message "2013-11-01 20:06:51" 'info "" "AlterSid est maintenant connu sous le nom Guest99037"))
                       "2013-11-01 20:06:51	--	AlterSid est maintenant connu sous le nom Guest99037")
         (check-equal? (message->weechat-string (message "2013-11-01 20:24:25" 'saying "LeoNerd" "(EVERYBODY LOVES SHOUTY 'LISP)"))
                       "2013-11-01 20:24:25	LeoNerd	(EVERYBODY LOVES SHOUTY 'LISP)")
         (check-equal? (message->weechat-string (message "2013-10-30 16:28:15" 'info "" "v0n est maintenant connu sous le nom v0n|w"))
                       "2013-10-30 16:28:15	--	v0n est maintenant connu sous le nom v0n|w"))
