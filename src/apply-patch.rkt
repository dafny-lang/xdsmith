;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket/base

(require racket/system
         racket/string
         racket/format
         racket/list)

(define (pr id #:user [user "dafny-lang"])
  (list user id))

;; PR ids
(define pr-ids (list (pr 1 #:user "sorawee") (pr 1392) (pr 1393)))
;; root dir relative to project dir, could also be absolute
(define root-dir (or (getenv "XDSMITH_ROOT") ".."))
;; project dir relative to root dir, could also be absolute
(define proj-dir (or (getenv "XDSMITH_PROJ") "dafny"))
;; paths of interest
(define paths '("Source" "Binaries"))

;; we are currently at the root dir

(for ([pr-id pr-ids])
  (system (format "wget https://github.com/~a/dafny/pull/~a.diff" (first pr-id) (second pr-id))))

(current-directory proj-dir)


(for ([pr-id pr-ids])
  (define pr-path-prefix (build-path root-dir (~a (second pr-id))))
  ;; project should be clean
  ;; (1) apply the patch
  (system (format "patch -p1 < ~a.diff" pr-path-prefix))
  ;; (2) extract only changes that we want (e.g., exclude tests)
  (system (format "git diff ~a > ~a-cleansed.diff" (string-join paths " ") pr-path-prefix))
  ;; (3) make the project clean again
  (system "git clean -f")
  (system "git reset --hard"))

;; now, just apply the patches
(for ([pr-id pr-ids])
  (define pr-path-prefix (build-path root-dir (~a (second pr-id))))
  (system (format "patch -p1 < ~a-cleansed.diff" pr-path-prefix)))
