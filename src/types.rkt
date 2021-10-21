;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket/base

(provide char-type

         set-type
         multiset-type
         method-type
         values-type

         method-type?

         fresh-comparable
         fresh-set-type
         fresh-multiset-type

         fresh-product-type
         product0-type
         product2-type
         product3-type

         map-value-type

         type->string)

(require racket/match
         racket/string
         xsmith
         xsmith/canned-components)

(define char-type (base-type 'char))

(define-generic-type set-type ([type covariant]))
(define-generic-type multiset-type ([type covariant]))
(define-generic-type method-type ([arg-type contravariant] [ret-type covariant]))

(define-generic-type product0-type ())
(define-generic-type product2-type (t1 t2))
(define-generic-type product3-type (t1 t2 t3))

(define (fresh-product-type)
  (fresh-type-variable (product0-type)
                       (product2-type (fresh-type-variable)
                                      (fresh-type-variable))
                       (product3-type (fresh-type-variable)
                                      (fresh-type-variable)
                                      (fresh-type-variable))))

;; this is a wrapper over product type
(define-generic-type values-type (t))

(define (fresh-comparable [n 2])
  (cond
    [(zero? n) (fresh-type-variable int-type bool-type char-type)]
    [else (fresh-type-variable
           int-type bool-type char-type
           (product0-type)
           (product2-type (fresh-comparable (sub1 n))
                          (fresh-comparable (sub1 n)))
           (product3-type (fresh-comparable (sub1 n))
                          (fresh-comparable (sub1 n))
                          (fresh-comparable (sub1 n)))
           (immutable (fresh-type-variable (set-type (fresh-comparable (sub1 n)))
                                           (multiset-type (fresh-comparable (sub1 n)))
                                           (array-type (fresh-comparable (sub1 n)))
                                           #;(dictionary-type (fresh-comparable (sub1 n)) (fresh-comparable (sub1 n))))))]))

(define (fresh-set-type) (set-type (fresh-comparable)))
(define (fresh-multiset-type) (multiset-type (fresh-comparable)))

(define (map-value-type)
  ;; disable until https://github.com/dafny-lang/dafny/issues/1372 is fixed
  #;(fresh-type-variable)
  (fresh-comparable))

(define (type->string t)
  (let loop ([t* t])
    (define t (concretize-type t*))
    (unify! t t*)
    (cond
      [(can-unify? t (product0-type))
       "()"]
      [(can-unify? t (product2-type (fresh-type-variable) (fresh-type-variable)))
       (define t1 (fresh-type-variable))
       (define t2 (fresh-type-variable))
       (unify! t (product2-type t1 t2))
       (format "(~a, ~a)" (loop t1) (loop t2))]
      [(can-unify? t (product3-type (fresh-type-variable) (fresh-type-variable) (fresh-type-variable)))
       (define t1 (fresh-type-variable))
       (define t2 (fresh-type-variable))
       (define t3 (fresh-type-variable))
       (unify! t (product3-type t1 t2 t3))
       (format "(~a, ~a, ~a)" (loop t1) (loop t2) (loop t3))]
      [(can-unify? t (immutable (array-type char-type)))
       (if (zero? (random 2)) "string" "seq<char>")]
      [(can-unify? t (immutable (array-type (fresh-type-variable))))
       (define inner (fresh-type-variable))
       (unify! t (immutable (array-type inner)))
       (format "seq<~a>" (loop inner))]
      [(can-unify? t (immutable (set-type (fresh-type-variable))))
       (define inner (fresh-type-variable))
       (unify! t (immutable (set-type inner)))
       (format "set<~a>" (loop inner))]
      [(can-unify? t (immutable (multiset-type (fresh-type-variable))))
       (define inner (fresh-type-variable))
       (unify! t (immutable (multiset-type inner)))
       (format "multiset<~a>" (loop inner))]
      [(can-unify? t (immutable (dictionary-type (fresh-type-variable) (fresh-type-variable))))
       (define inner-1 (fresh-type-variable))
       (define inner-2 (fresh-type-variable))
       (unify! t (immutable (dictionary-type inner-1 inner-2)))
       (format "map<~a, ~a>" (loop inner-1) (loop inner-2))]
      [(can-unify? t (product-type #f))
       (format "(~a)" (string-join (map loop (product-type-inner-type-list t)) ", "))]
      [(can-unify? t (function-type (fresh-type-variable) (fresh-type-variable)))
       (define ret (fresh-type-variable))
       (define arg (fresh-type-variable))
       (unify! t (function-type arg ret))
       (format "(~a -> ~a)" (loop arg) (loop ret))]
      [(can-unify? t number-type) "int"]
      [(can-unify? t int-type) "int"]
      [(can-unify? t bool-type) "bool"]
      [(can-unify? t char-type) "char"]
      [else (error 'type->string "Type not implemented yet: ~v" t)])))
