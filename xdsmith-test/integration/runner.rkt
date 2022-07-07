;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket

(module+ test
  (require rackunit
           xdsmith/oracle)

  (define (obtain-paths dir)
    (for/list ([path (directory-list dir #:build? #t)]
               #:when (path-has-extension? path #".dfy"))
      (simple-form-path path)))

  (define good-paths (obtain-paths "good"))
  (define bad-paths (obtain-paths "bad"))

  (current-directory "../../work-dir")

  ;; Testing the oracle (known good file)

  (for ([good-path good-paths])
    (with-check-info (['path good-path])
      (check-equal? (oracle good-path) 'ok)))

  ;; Testing the oracle (known bad files)

  (for ([bad-path bad-paths])
    (with-check-info (['path bad-path])
      (check-equal? (oracle bad-path) 'known-mismatch)))

  ;; End-to-end test with a fixed seed

  (define tmp.dfy (make-temporary-file "tmp~a.dfy"))
  (with-output-to-file tmp.dfy
    #:exists 'replace
    (λ ()
      (system* (find-executable-path "racket")
               "../xdsmith/fuzzer.rkt"
               "--seed" "42"
               "--dafny-syntax" "true")))
  (displayln "== Example generated file ==")
  (display (file->string tmp.dfy))

  (check-true
   (system* (find-executable-path "racket")
            "../xdsmith/differ.rkt"
            tmp.dfy)))
