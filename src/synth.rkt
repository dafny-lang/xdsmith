;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang rosette

(provide synth:pos synth:neg)
(require rosette/lib/destruct
         (only-in clotho random))

(struct TheVar () #:transparent)
(struct IntLiteral (v) #:transparent)
(struct Plus (l r) #:transparent)
(struct Minus (l r) #:transparent)
(struct Equal (l r) #:transparent)
(struct LessThan (l r) #:transparent)
(struct And (l r) #:transparent)
(struct Or (l r) #:transparent)

(define (pp e)
  (λ (the-hole)
    (let loop ([e e])
      (destruct e
        [(TheVar) the-hole]
        [(IntLiteral v) `(IntLiteral ,v)]
        [(Plus a b) `(Plus ,(loop a) ,(loop b))]
        [(Minus a b) `(Minus ,(loop a) ,(loop b))]
        [(Equal a b) `(Equal ,(loop a) ,(loop b))]
        [(LessThan a b) `(LessThan ,(loop a) ,(loop b))]
        [(And a b) `(And ,(loop a) ,(loop b))]
        [(Or a b) `(Or ,(loop a) ,(loop b))]))))

(define (synth:core v f)
  (clear-state!)
  (define count 0)
  (define (interp e)
    (destruct e
      [(TheVar)
       (set! count (add1 count))
       v]
      [(IntLiteral v) v]
      [(Plus l r) (+ (interp l) (interp r))]
      [(Minus l r) (- (interp l) (interp r))]
      [(Equal l r) (= (interp l) (interp r))]
      [(LessThan l r) (< (interp l) (interp r))]
      [(And l r) (&& (interp l) (interp r))]
      [(Or l r) (|| (interp l) (interp r))]))
  (define (?expr depth random-depth)
    (define (?expr depth)
      (define (make-choice c)
        (define-symbolic* n integer?)
        (cond
          [(= c 0) (TheVar)]
          [(= c 1) (IntLiteral n)]
          [(= c 2) (Plus (?expr (sub1 depth)) (?expr (sub1 depth)))]
          [(= c 3) (Minus (?expr (sub1 depth)) (?expr (sub1 depth)))]
          [(= c 4) (Equal (?expr (sub1 depth)) (?expr (sub1 depth)))]
          [(= c 5) (LessThan (?expr (sub1 depth)) (?expr (sub1 depth)))]
          [(= c 6) (And (?expr (sub1 depth)) (?expr (sub1 depth)))]
          [else (Or (?expr (sub1 depth)) (?expr (sub1 depth)))]))
      (cond
        [(zero? depth) (assert #f)]
        [(<= depth (add1 random-depth))
         (define-symbolic* c integer?)
         (make-choice c)]
        [else
         (make-choice (random 8))]))
    (?expr depth))

  (define sketch (?expr 4 (random 4)))
  (define sol (solve (let ([interped (interp sketch)])
                       (assert (positive? count))
                       (assert (boolean? interped))
                       (assert (f interped)))))
  (cond
    [(unsat? sol) (synth:core v f)]
    [else (pp (evaluate sketch sol))]))

(define (synth:pos v)
  (synth:core v values))

(define (synth:neg v)
  (synth:core v not))
