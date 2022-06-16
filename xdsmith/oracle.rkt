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

(provide oracle get-result)
(require "basic.rkt"
         "normalize.rkt")

(define dafny-path (or (getenv "DAFNYPATH") "/workspace/dafny/Binaries/Dafny"))

;; extract-ans :: path-string? string? string? ...
;;                #:norm (-> string? string?)
;;                #:name (or/c #f string?) = #f
;;                #:verify? boolean? = #t
;;                -> ans?
;; @exn xdsmith-error:run
;;
;; Compiles and runs a Dafny program at `path` via the `target` language backend.
;; The name of the language backend can be further refined by the
;; `name` parameters.
;; If the run terminates normally, the output is normalized via `normalizer`.
;; If `verify?` is true, then the compilation also verifies that the Dafny program
;; satisfies its specification.
;; Abnormally terminated programs (compilation error, verification error)
;; will cause a `xdsmith-error:run` exception.
(define (extract-ans path target
                     #:norm normalizer
                     #:name [name target]
                     #:verify? [verify? #t]
                     . other-args*)
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
    (raise (xdsmith-error:bad-compilation target out-str err-str)))
  (match (regexp-match #px"^(.*?)Running\\.\\.\\.\n\n(.*)$" out-str)
    [(list _ _ the-ans) (ans (normalizer the-ans) name)]
    [_ (raise (xdsmith-error:unexpected-output name out-str err-str))]))

(define (known-mismatch)
  'known-mismatch)

(define (known-mismatch-handler e)
  (match e
    [(xdsmith-error:bad-compilation name out err)
     (cond
       ;; actually from Dafny verification executed as a part of JS run
       [(and (equal? name "js")
             (string-contains?
              err
              "Process terminated. Assumption failed."))
        (known-mismatch)]
       [(and (equal? name "js")
             (string-contains?
              out
              "Error: the type of this expression is underspecified"))
        (known-mismatch)]
       [(and (equal? name "js")
             (string-contains?
              out
              "Error: All elements of display must have some common supertype"))
        (known-mismatch)]

       ;; from non-verification
       [(and (equal? name "cs")
             (string-contains?
              out
              "error CS0305: Using the generic type 'Func<TResult>' requires 1 type arguments"))
        (known-mismatch)]
       [(and (equal? name "java")
             (string-contains?
              out
              "error: local variables referenced from a lambda expression must be final or effectively final"))
        (known-mismatch)]
       [else (raise e)])]
    [_ (raise e)]))

;; oracle :: path-string? -> (or/c 'ok 'known-mismatch)
;; @exn xdsmith-error:run, xdsmith-error:mismatch
;;
;; Runs the Dafny program at `path` with various backends
;; and compares their outputs. If the outputs don't agree,
;; raise the xdsmith-error:mismatch exception.
(define (oracle path)
  (with-handlers ([xdsmith-error:run? known-mismatch-handler])
    (define ans-js (extract-ans path "js"
                                #:norm normalize-js
                                #:name "JS"))
    (define ans-go (extract-ans path "go"
                                #:norm normalize-go
                                #:name "Go"
                                #:verify? #f))
    (define ans-cs (extract-ans path "cs"
                                #:norm normalize-cs
                                #:name "C#"
                                #:verify? #f))
    (define ans-java (extract-ans path "java"
                                  #:norm normalize-java
                                  #:name "Java"
                                  #:verify? #f))
    (define ans-cs* (extract-ans path "cs" "/optimize"
                                 #:norm normalize-cs
                                 #:name "C# (optimized)"
                                 #:verify? #f))

    (define outs (list ans-js ans-go ans-cs ans-java ans-cs*))
    (define outs* (map ans-out outs))
    (unless (for/and ([out (rest outs*)])
              (equal? (first outs*) out))
      (raise (xdsmith-error:mismatch outs)))
    'ok))

;; get-result :: path-string? -> (or/c ans? 'known-mismatch)
;; @exn xdsmith-error:run
;;
;; Run a Dafny file `path` and return its output
(define (get-result path)
  (with-handlers ([xdsmith-error:run? known-mismatch-handler])
    (extract-ans path "js" #:norm values)))
