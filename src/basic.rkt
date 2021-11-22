;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket/base

(provide current-print-style
         current-check-all?
         (struct-out xdsmith-error)
         (struct-out xdsmith:bad-compilation)
         (struct-out xdsmith:unexpected-output))

(define current-print-style (make-parameter 'readable))
(define current-check-all? (make-parameter #f))

(struct xdsmith-error () #:transparent)
(struct xdsmith:bad-compilation xdsmith-error (target out err) #:transparent)
(struct xdsmith:unexpected-output xdsmith-error (target out err) #:transparent)
