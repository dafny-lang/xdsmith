;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket

(require syntax/location
         "basic.rkt"
         "oracle.rkt"
         "pretty.rkt")

(module gen racket
  (require "fuzzer.rkt")
  (provide gen)
  (define (gen path)
    (define s
      (with-output-to-string
        (λ () (parameterize ([current-command-line-arguments #()])
                (dafny-command-line)))))
    (define p (open-input-string s))
    (define seed-info (read-line p))
    (define datum (read p))
    (values seed-info datum)))

(define path-to-gen (quote-module-path gen))

(module+ main
  (define current-limit +inf.0)
  (define current-batch-size 5)
  (command-line
   #:once-each
   [("--print-blob") "Print string blob"
                     (current-print-style 'blob)]
   [("--check-all") "Check all backends"
                    (current-check-all? #t)]
   [("--limit") num-limit "Iteration limit (default: none)"
                (set! current-limit (string->number num-limit))]
   [("--size") batch-size "Batch size (default: 10)"
               (set! current-batch-size (string->number batch-size))]
   #:args ([s #f])
   (cond
     [s (oracle s)]
     [else
      (define path "tmp.dfy")
      (for ([i (in-range current-limit)])
        (parameterize ([current-namespace (make-base-namespace)])
          (define gen (dynamic-require path-to-gen 'gen))
          (for ([j (in-range current-batch-size)])
            (define-values (seed-info datum) (gen path))
            (displayln seed-info)
            (with-output-to-file path #:exists 'replace
              (λ ()
                (displayln seed-info)
                (display (main-print datum))))
            (oracle path))))])))
