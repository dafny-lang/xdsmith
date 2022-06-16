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

  (current-directory "../../workspace")

  (for ([good-path good-paths])
    (with-check-info (['path good-path])
      (check-equal? (oracle good-path) 'ok)))

  (for ([bad-path bad-paths])
    (with-check-info (['path bad-path])
      (check-equal? (oracle bad-path) 'known-mismatch))))
