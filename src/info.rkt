;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang info
(define collection "xdsmith")
(define deps '("base" "clotho" "xsmith" "racr" "pprint" "rosette"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/xdsmith.scrbl" ())))
(define pkg-desc "Fuzzing Dafny")
(define version "0.0")
(define pkg-authors '(sorawee))
