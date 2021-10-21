;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket/base

(provide debug
         pretty-print-children
         binary-op-renderer
         define-nary-op
         define-chain-op)

(require racket/list
         syntax/parse/define
         (except-in xsmith module)
         xsmith/app
         racr
         (only-in clotho random random-ref)
         (for-syntax racket/list
                     racket/base))

(module helper racket/base
  (provide children-names)
  (define children-names '(one two three four five six seven eight nine ten)))

(require 'helper
         (for-syntax 'helper))

(define (debug . xs)
  (xd-printf "~a\n" xs)
  (last xs))

(define (pretty-print-children cns)
  (map (λ (cn) ($xsmith_render-node cn)) (ast-children cns)))

(define ((binary-op-renderer variant) n)
  (list variant
        ($xsmith_render-node (ast-child 'l n))
        ($xsmith_render-node (ast-child 'r n))))

(define ((chain-op-renderer variant) n)
  (list variant
        (pretty-print-children (ast-child 'e n))
        (ast-child 'op n)))

(define ((nary-op-renderer name num) n)
  (cons name
        (for/list ([i (in-range num)] [field (in-list children-names)])
          ($xsmith_render-node (ast-child field n)))))

(define ((nary-children-type-wrapper f) n t)
  (for/hash ([child-type (in-list (f n t))] [name (in-list children-names)])
    (values name child-type)))

(define ((chain-children-type-wrapper f) n t)
  (hash 'e (f n t)))

(define-syntax-parse-rule (define-nary-op comp-name:id [name:id #:arity n:nat #:type expected-type children-type rst ...] ...)
  #:fail-when
  (for/or ([n (attribute n)])
    (or (< (syntax-e n) 0)
        (> (syntax-e n) 10)))
  "must be between 0 and 10 (inclusive)"

  #:with ((field ...) ...) (map (λ (n) (take children-names (syntax-e n))) (attribute n))
  (add-to-grammar
   comp-name
   [name
    Expression ([field : Expression] ...)
    #:prop type-info
    [expected-type (nary-children-type-wrapper children-type)]
    #:prop render-node-info
    (nary-op-renderer 'name n)
    rst ...] ...))

(define-syntax-parse-rule (define-chain-op comp-name:id [name:id #:op ops #:type expected-type children-type] ...)
  (add-to-grammar
   comp-name
   [name
    Expression ([e : Expression *] [op])
    #:prop fresh
    (let ([len (random 2 4)])
      (hash 'e len
            'op (for/list ([i (sub1 len)])
                  (random-ref ops))))
    #:prop type-info
    [expected-type (chain-children-type-wrapper children-type)]
    #:prop render-node-info
    (chain-op-renderer 'name)] ...))
