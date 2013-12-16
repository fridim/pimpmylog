#lang racket/base

(struct message (date type nick msg) #:transparent)

(provide (struct-out message))
