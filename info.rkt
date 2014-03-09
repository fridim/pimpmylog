#lang setup/infotab

(define version "0.1")
(define collection "pimpmylog")

(define deps '("base"
               "srfi-lite-lib"
               "web-server-lib"))

(define build-deps '("rackunit-lib"))

(define raco-commands '(("pimpmylog" (submod pimpmylog main) "run pimpmylog" #f)))
