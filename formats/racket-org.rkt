#lang racket/base

(provide racket-org-string->message)

(require "../message.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket-org log format
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->message re type)
  (let ([date (cadr re)]
        [nick (caddr re)]
        [msg (cadddr re)])
    (message date type nick msg)))

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
         (require rackunit)
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