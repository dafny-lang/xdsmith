;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket/base

(require racket/match
         racket/format
         racket/list)

(provide display-xdsmith-error

         current-print-style
         current-check-all?
         (struct-out xdsmith-error)

         (struct-out xdsmith-error:run)
         (struct-out xdsmith-error:bad-compilation)
         (struct-out xdsmith-error:unexpected-output)

         (struct-out xdsmith-error:mismatch)

         (struct-out xdsmith-error:unexpected)

         (struct-out ans)

         log-xdsmith-logger-error
         log-xdsmith-logger-info)

;; either 'readable or 'blob
(define current-print-style (make-parameter 'readable))

;; either #f or #t
(define current-check-all? (make-parameter #f))

(struct xdsmith-error () #:transparent)

(struct xdsmith-error:run xdsmith-error (target out err) #:transparent)
(struct xdsmith-error:bad-compilation xdsmith-error:run () #:transparent)
(struct xdsmith-error:unexpected-output xdsmith-error:run () #:transparent)

(struct xdsmith-error:mismatch xdsmith-error (xs) #:transparent)

(struct xdsmith-error:unexpected xdsmith-error (expected got) #:transparent)

(struct ans (out name) #:transparent)

(define sep "******************")

(define (display-output answer)
  (match-define (ans out name) answer)
  (printf "backend: ~a\n" name)
  (match (current-print-style)
    ['blob (printf "~v" out)]
    ['readable
     (displayln (~a sep "\n" out "\n" sep))])
  (newline))

(define (display-xdsmith-error e)
  (match e
    [(xdsmith-error:bad-compilation target out err)
     (printf "Compilation error for ~a\n" target)
     (displayln "Output:")
     (displayln out)
     (displayln "Error:")
     (displayln err)
     (newline)]
    [(xdsmith-error:unexpected-output target out err)
     (printf "Unexpected output for ~a\n" target)
     (displayln "Output:")
     (displayln out)
     (displayln "Error:")
     (displayln err)
     (newline)]
    [(xdsmith-error:mismatch xs)
     (cond
       [(current-check-all?)
        (displayln "=== Full outputs ===")
        (for ([ans xs])
          (display-output ans))]
       [else
        (displayln "=== Partial outputs ===")
        (display-output (first xs))
        (display-output
         (for/first ([ans xs]
                     #:unless (equal? (ans-out ans) (ans-out (first xs))))
           ans))])]))

(define-logger xdsmith-logger)
