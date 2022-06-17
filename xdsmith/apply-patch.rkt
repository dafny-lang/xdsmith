;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

;; This script applies bug-fixing patches that are not yet merged to the
;; upstream Dafny so that we don't discover issues that we have already
;; discovered. It is automatically run in the Docker.
;; Run racket apply-patch.rkt --help for help

#lang racket/base

(require (only-in racket/system [system s:system])
         racket/string
         racket/format
         racket/cmdline
         racket/path
         racket/list)

(define (pr id #:user [user "dafny-lang"])
  (list user id))

;; PR ids
(define pr-ids (list (pr 1 #:user "sorawee")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (working-dir dafny-dir)
  (command-line
   #:usage-help "<working-dir> is a path to the directory where patches \
will be downloaded to"
   #:usage-help "<dafny-dir> is a path to the Dafny project directory \
(which should contain subdirectories like Source and Binaries)"
   #:args (working-dir dafny-dir)
   (values (simple-form-path working-dir)
           (simple-form-path dafny-dir))))

(define (system . xs)
  (unless (apply s:system xs)
    (printf "The command ~a returns non-zero status\n" xs)
    (exit 1)))

;; Paths of interest. Exclude other paths to minimize potential merge conflicts.
(define paths '("Source" "Binaries"))

(current-directory working-dir)

(for ([pr-id pr-ids])
  (system (format "wget https://github.com/~a/dafny/pull/~a.diff"
                  (first pr-id)
                  (second pr-id))))

(current-directory dafny-dir)

(for ([path paths])
  (unless (directory-exists? path)
    (raise-user-error "<dafny-dir> is not pointing to the Dafny project. The value is:" dafny-dir)))

(for ([pr-id pr-ids])
  (define pr-path-prefix (build-path working-dir (~a (second pr-id))))
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
  (define pr-path-prefix (build-path working-dir (~a (second pr-id))))
  (system (format "patch -p1 < ~a-cleansed.diff" pr-path-prefix))
  (delete-file (format "~a.diff" pr-path-prefix))
  (delete-file (format "~a-cleansed.diff" pr-path-prefix)))
