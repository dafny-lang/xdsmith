;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

;; This module provides an oracle for differential testing and
;; a function to obtain output from a Dafny program.
;;
;; Environment variable:
;; - DAFNYPATH is a path to the Dafny executable.
;;   By default, it is /workspace/dafny/Binaries/Dafny, which is the path where
;;   Dafny locates in the Docker environment.

#lang racket

(provide oracle get-result (struct-out ans))
(require "basic.rkt"
         "normalize.rkt")

(define dafny-path (or (getenv "DAFNYPATH") "/workspace/dafny/Binaries/Dafny"))

(struct ans (out name) #:transparent)

(define (extract-ans #:norm normalizer #:name [name #f] #:verify? [verify? #t]
                     path target . other-args*)
  (define other-args
    (cond
      [verify? (list* "/compile:3" other-args*)]
      [else (list* "/compile:4" "/noVerify" other-args*)]))

  (define-values (in-for-out out-port) (make-pipe))
  (define-values (in-for-err err-port) (make-pipe))
  (define exit-ok?
    (parameterize ([current-output-port out-port]
                   [current-error-port err-port])
      (begin0 (apply system*
                     dafny-path
                     path
                     (format "/compileTarget:~a" target)
                     other-args)
        (close-output-port out-port)
        (close-output-port err-port))))
  (define out-str (port->string in-for-out))
  (define err-str (port->string in-for-err))
  (unless exit-ok?
    (raise (xdsmith:bad-compilation target out-str err-str)))
  (match (regexp-match #px"^(.*?)Running\\.\\.\\.\n\n(.*)$" out-str)
    [(list _ _ the-ans) (ans (normalizer the-ans) name)]
    [_ (raise (xdsmith:unexpected-output target out-str err-str))]))

(define (known-mismatch)
  (displayln "Known mismatch or error!")
  'known-mismatch)

(define ((handler expect?) e)
  (match e
    [(? expect?) 'error]
    [(xdsmith:bad-compilation target out err)
     (displayln (format "Compilation error for ~a" target))
     (displayln "Output:")
     (displayln out)
     (displayln "Error:")
     (displayln err)
     (newline)
     (cond
       [(string-contains? (xdsmith:bad-compilation-err e)
                          "Process terminated. Assumption failed.")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "error CS0305: Using the generic type 'Func<TResult>' requires 1 type arguments")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "Error: the type of this expression is underspecified")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "error: local variables referenced from a lambda expression must be final or effectively final")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "Error: All elements of display must have some common supertype")
        (known-mismatch)]
       [else (displayln "New result!")
             (exit 1)])]
    [(xdsmith:unexpected-output target out err)
     (displayln (format "Unexpected output for ~a" target))
     (displayln "Output:")
     (displayln out)
     (displayln "Error:")
     (displayln err)
     (newline)
     (exit 1)]
    [_ (raise e)]))

(define (display-output s)
  (match (current-print-style)
    ['blob (printf "~v" s)]
    ['readable
     (define sep "******************")
     (displayln (~a sep "\n" s "\n" sep))]))

(define (raise-mismatch-error)
  (raise-user-error "Mismatch!"))

(define (check-pair a b)
  (unless (or (current-check-all?) (equal? (ans-out a) (ans-out b)))
    (printf "~a:\n" (ans-name a))
    (display-output (ans-out a))
    (newline)
    (printf "~a:\n" (ans-name b))
    (display-output (ans-out b))
    (newline)
    (raise-mismatch-error)))

(define (check-all . xs)
  (when (current-check-all?)
    (define outs (map ans-out xs))
    (unless (for/and ([out (rest outs)])
              (equal? (first outs) out))
      (for ([ans xs])
        (printf "~a:\n" (ans-name ans))
        (display-output (ans-out ans))
        (newline))
      (raise-mismatch-error))))

(define (oracle path)
  (with-handlers ([xdsmith-error? (handler (λ (_) #f))])
    (define ans-js (extract-ans path "js"
                                #:norm normalize-js
                                #:name "JS"))
    (define ans-go (extract-ans path "go"
                                #:norm normalize-go
                                #:name "Go"
                                #:verify? #f))
    (check-pair ans-js ans-go)
    (define ans-cs (extract-ans path "cs"
                                #:norm normalize-cs
                                #:name "C#"
                                #:verify? #f))
    (check-pair ans-js ans-cs)
    (define ans-java (extract-ans path "java"
                                  #:norm normalize-java
                                  #:name "Java"
                                  #:verify? #f))
    (check-pair ans-js ans-java)
    (define ans-cs* (extract-ans path "cs" "/optimize"
                                 #:norm normalize-cs
                                 #:name "C# (optimized)"
                                 #:verify? #f))
    (check-pair ans-js ans-cs*)
    (check-all ans-js ans-go ans-cs ans-java ans-cs*)))

;; get-result :: path-string? (exn? -> boolean?) -> any/c
;; Run a Dafny file `path` and return its output or errors with the xdsmith-error
;; `handling-error?` is used to determined if we want to handle an error or not.
;; If we are handling the error, then the result could be either:
;;   - 'error (it's an expected verification error)
;;   - any/c
;; Otherwise, the result could be either:
;;   - 'known-mismatch
;;   - ans?
(define (get-result path handling-error?)
  (with-handlers ([xdsmith-error? (handler handling-error?)])
    (extract-ans path "js" #:norm values)))
