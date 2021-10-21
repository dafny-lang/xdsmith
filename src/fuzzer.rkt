;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang clotho

(provide dafny-command-line)

(require xsmith
         racr
         xsmith/racr-convenience
         xsmith/canned-components
         xsmith/app

         racket/match
         racket/string
         racket/pretty
         (only-in racket/set set-subtract)
         racket/list
         racket/hash
         "pretty.rkt"
         "lib.rkt"
         "types.rkt")

(define current-dafny-syntax? (make-parameter #f))

(define type-table (make-hash))

(define (depth-weight #:shallow [shallow-weight 1]
                      #:deep [deep-weight 3])
  (λ (n) (if (att-value 'xsmith_at-max-depth? n)
             deep-weight
             shallow-weight)))

(define (fuzzing-verifier?)
  (xsmith-feature-enabled? 'print-constrained))


(define-basic-spec-component dafny-comp)

(define chars (set-subtract (range 33 127)
                            (range 48 58) ;; no numbers
                            (map char->integer
                                 '(#\{ #\} #\[ #\] #\( #\) #\, #\` #\# #\. #\\))))

;; this ensures that strings will have length at least 2,
;; which is a property that we want because in the Java backend
;; ["ab"] is printed as [[a, b]], so we need to normalize it back to
;; [ab]. However, if a one character string is allowed,
;; then [[a, b]] could be legitimately produced by [["a", "b"]],
;; confusing the normalization.
;;
;; Also don't generate too long strings, at least until
;; https://github.com/dafny-lang/dafny/issues/1362
;; is fixed

(define (random-char/ascii-no-control)
  (random-char-in-range chars))

(define (random-string/ascii-no-control)
  (random-string-from-char-producing-proc
   random-char/ascii-no-control
   40
   #f
   random-char/ascii-no-control))

(add-basic-expressions dafny-comp
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:LambdaWithExpression #t
                       #:Numbers #t
                       #:Booleans #t
                       #:ImmutableArray #t
                       ;; NOTE: disable until
                       ;; https://github.com/dafny-lang/dafny/issues/1297
                       ;; is fixed
                       #;#;#:LetSequential #t)

(add-basic-statements dafny-comp
                      #:Block #t
                      #:IfElseStatement #t
                      #:AssignmentStatement #t)

(define (lambda-fresh-implementation cur-hole make-fresh-node-func)
  (define arg-type (product-type #f))
  (define ret-type (product-type #f))
  (define type ($xsmith_type cur-hole))
  (define ftype (method-type arg-type ret-type))
  (unify! ftype type)
  (force-type-exploration-for-node! cur-hole)

  (define parameters
    (map (λ (t) (make-fresh-node-func 'FormalParameter (hash 'type t)))
         (or (product-type-inner-type-list arg-type)
             (for/list ([x (random 6)])
               (if (fuzzing-verifier?)
                   int-type
                   (fresh-type-variable))))))

  (define rets
    (map (λ (t) (make-fresh-node-func 'FormalParameter (hash 'type t)))
         (or (product-type-inner-type-list ret-type)
             (for/list ([x (random 6)])
               (if (fuzzing-verifier?)
                   int-type
                   (fresh-type-variable))))))

  (unify! (product-type (map (λ (x) (ast-child 'type x))
                             parameters))
          arg-type)
  (unify! (product-type (map (λ (x) (ast-child 'type x))
                             rets))
          ret-type)
  (hash 'type type
        'parameters parameters
        'rets rets))

(add-choice-method
 dafny-comp
 method-id-first-order
 [#f (λ () #t)]
 [VariableReference
  (λ ()
    (define t ($xsmith_type (current-hole)))
    (define method? (can-unify? t (method-type (fresh-type-variable) (fresh-type-variable))))
    (cond
      [method?
       (and (parent-node (current-hole))
            (equal? (ast-node-type (parent-node (current-hole)))
                    'ProcedureApplicationForMethod))]
      ;; we are generating default return values.
      ;; at this point, there's no lifting target, so we can't have
      ;; variable reference
      [(and (parent-node (current-hole))
            (equal? (ast-node-type (parent-node (current-hole)))
                    'TopLevelMethod))
       #f]
      [else #t]))])

(add-property dafny-comp
              choice-filters-to-apply
              [VariableReference (method-id-first-order)])

(add-to-grammar
 dafny-comp
 [ProgramWithMain
  #f ([decls : TopLevelDefinition *]
      [body : Block])
  #:prop block-user? #t
  #:prop strict-child-order? #t
  #:prop type-info
  [(fresh-type-variable)
   (λ (n t)
     (hash 'decls (λ (c) (fresh-type-variable
                          (function-type (product-type #f)
                                         (fresh-type-variable))
                          (method-type (product-type #f)
                                       (product-type #f))))
           'body (λ (c) no-return-type)))]
  #:prop render-node-info
  (λ (n)
    `(ProgramWithMain
      ,(pretty-print-children (ast-child 'decls n))
      ,($xsmith_render-node (ast-child 'body n))))]

 [TopLevelDefinition #f () #:prop may-be-generated #f]
 [TopLevelMethodDefinition
  TopLevelDefinition (name type TopLevelMethod)
  #:prop binder-info ()
  #:prop type-info
  [(method-type (product-type #f) (product-type #f))
   (λ (n t) (hash 'TopLevelMethod t))]
  #:prop render-node-info
  (λ (n)
    `(TopLevelMethodDefinition
      ,(ast-child 'name n)
      ,($xsmith_render-node (ast-child 'TopLevelMethod n))))
  #:prop fresh
  (let ()
    (define t (method-type (product-type #f) (product-type #f)))
    (unify! ($xsmith_type (current-hole)) t)
    (hash 'name (fresh-var-name "method_")
          'type t))]
 [TopLevelFunMethodDefinition
  TopLevelDefinition (name type TopLevelFunMethod)
  #:prop binder-info ()
  #:prop type-info
  [(function-type (product-type #f) (fresh-type-variable))
   (λ (n t) (hash 'TopLevelFunMethod t))]
  #:prop render-node-info
  (λ (n)
    `(TopLevelFunMethodDefinition
      ,(ast-child 'name n)
      ,($xsmith_render-node (ast-child 'TopLevelFunMethod n))))
  #:prop fresh
  (let ()
    (define t (function-type (product-type #f) (fresh-type-variable)))
    (unify! ($xsmith_type (current-hole)) t)
    (hash 'name (fresh-var-name "fun_method_")
          'type t))]
 [TopLevelMethod
  #f ([parameters : FormalParameter * = (random 6)]
      [rets : FormalParameter * = (random 6)]
      [retsinit : Expression * = (create-ast-bud)]
      [body : Block])
  #:prop edit
  (λ (n)
    (and (ast-bud-node? (ast-child 'retsinit n))
         ($xsmith_no-holes-in-subtree? (ast-child 'rets n))
         (λ () (rewrite-subtree (ast-child 'retsinit n)
                                (create-ast-list
                                 (for/list ([child (ast-children (ast-child 'rets n))])
                                   (make-hole 'Expression)))))))

  #:prop block-user? #t
  #:prop wont-over-deepen #t
  #:prop type-info
  [(method-type (product-type #f) (product-type #f))
   (λ (n t)
     (define args-type (product-type
                        (map (λ (x) (fresh-type-variable))
                             (ast-children (ast-child 'parameters n)))))
     (define rets-type (product-type
                        (map (λ (x) (fresh-type-variable))
                             (ast-children (ast-child 'rets n)))))
     (unify! (method-type args-type rets-type)
             t)
     (define args-list (product-type-inner-type-list args-type))
     (define rets-list (product-type-inner-type-list rets-type))
     (define partial
       (hash-set
        (for/hash ([c (in-sequences (ast-children (ast-child 'parameters n))
                                    (ast-children (ast-child 'rets n)))]
                   [at (in-sequences args-list rets-list)])
          (values c at))
        'body no-return-type))
     (cond
       [(ast-bud-node? (ast-child 'retsinit n)) partial]
       [else (hash-union partial
                         (for/hash ([ret-type (product-type-inner-type-list! rets-type)]
                                    [retinit (ast-children (ast-child 'retsinit n))])
                           (values retinit ret-type)))]))]

  #:prop fresh
  (lambda-fresh-implementation (current-hole) make-fresh-node)
  #:prop render-node-info
  (λ (n)
    `(TopLevelMethod
      ,(pretty-print-children (ast-child 'parameters n))
      ,(pretty-print-children (ast-child 'rets n))
      (BoolLiteral #t)
      (BoolLiteral #t)
      ,(pretty-print-children (ast-child 'retsinit n))
      ,($xsmith_render-node (ast-child 'body n))))]

 [TopLevelFunMethod
  #f ([definitions : Definition *]
      LambdaWithExpression)
  #:prop type-info
  [(function-type (product-type #f) (fresh-type-variable))
   (λ (n t)
     (hash 'LambdaWithExpression t
           'definitions (λ (cn) (fresh-type-variable))))]
  #:prop render-node-info
  (λ (n)
    (define ret-type (fresh-type-variable))
    (define ft (function-type (product-type #f) ret-type))
    (define the-type (concretize-type ($xsmith_type n)))
    (unify! the-type ft)

    `(TopLevelFunMethod
      ,(pretty-print-children (ast-child 'parameters (ast-child 'LambdaWithExpression n)))
      ,(type->string ret-type)
      ,(pretty-print-children (ast-child 'definitions n))
      ,($xsmith_render-node (ast-child 'body (ast-child 'LambdaWithExpression n)))))]

 [MethodBlock
  Statement ([e : ProcedureApplicationForMethod]
             [elem : DefinitionNoRhs = (create-ast-bud)]
             [body : Block = (create-ast-bud)])
  #:prop binding-structure 'serial/all
  #:prop strict-child-order? #t
  #:prop edit
  ;; Fill in elemname after collection
  (λ (n)
    (and (ast-bud-node? (ast-child 'elem n))
         ($xsmith_no-holes-in-subtree? (ast-child 'e n))
         (let* ([collection-type (att-value 'xsmith_type (ast-child 'e n))]
                [inner-type (fresh-type-variable)]
                [_void (unify! (values-type (product-type (list inner-type)))
                               collection-type)]
                ;; We have to force type exploration so that the type
                ;; field of the DefinitionNoRhs will match the
                ;; collection type properly.
                [_void (force-type-exploration-for-node!
                        (ast-child 'e n))]
                [new-def (make-fresh-node
                          'DefinitionNoRhs
                          (hash 'name (fresh-var-name "methoddefvar_")
                                'type (concretize-type inner-type)))])
           (λ () (rewrite-subtree (ast-child 'elem n)
                                  new-def)))))
  #:prop edit
  ;; Fill in body after elemname
  (λ (n)
    (and (ast-bud-node? (ast-child 'body n))
         (not (ast-bud-node? (ast-child 'elem n)))
         (not ($xsmith_is-hole? (ast-child 'elem n)))
         (λ () (rewrite-subtree (ast-child 'body n)
                                (make-hole 'Block)))))

  #:prop type-info
  [no-return-type
   (λ (n t)
     (define t (fresh-type-variable))
     (if (fuzzing-verifier?)
         (hash 'elem int-type
               'e (values-type (product-type (list int-type)))
               'body no-return-type)
         (hash 'elem t
               'e (values-type (product-type (list t)))
               'body no-return-type)))]
  #:prop render-node-info
  (λ (n)
    `(MethodBlock ,(ast-child 'name (ast-child 'elem n))
                  ,($xsmith_render-node (ast-child 'e n))
                  ,($xsmith_render-node (ast-child 'body n))))]

 [MethodBlockTwo
  Statement ([e : ProcedureApplicationForMethod]
             [elem : DefinitionNoRhs = (create-ast-bud)]
             [elemtwo : DefinitionNoRhs = (create-ast-bud)]
             [body : Block = (create-ast-bud)])
  #:prop binding-structure 'serial/all
  #:prop strict-child-order? #t
  #:prop edit
  ;; Fill in elemname after collection
  (λ (n)
    (and (ast-bud-node? (ast-child 'elem n))
         ($xsmith_no-holes-in-subtree? (ast-child 'e n))
         (let* ([collection-type (att-value 'xsmith_type (ast-child 'e n))]
                [inner-type-1 (fresh-type-variable)]
                [inner-type-2 (fresh-type-variable)]
                [_void (unify! (values-type (product-type (list inner-type-1 inner-type-2)))
                               collection-type)]
                ;; We have to force type exploration so that the type
                ;; field of the DefinitionNoRhs will match the
                ;; collection type properly.
                [_void (force-type-exploration-for-node!
                        (ast-child 'e n))]
                [new-def-1 (make-fresh-node
                            'DefinitionNoRhs
                            (hash 'name (fresh-var-name "methoddefvar_")
                                  'type (concretize-type inner-type-1)))]
                [new-def-2 (make-fresh-node
                            'DefinitionNoRhs
                            (hash 'name (fresh-var-name "methoddefvar_")
                                  'type (concretize-type inner-type-2)))])
           (λ ()
             (rewrite-subtree (ast-child 'elem n)
                              new-def-1)
             (rewrite-subtree (ast-child 'elemtwo n)
                              new-def-2)))))
  #:prop edit
  ;; Fill in body after elemname
  (λ (n)
    (and (ast-bud-node? (ast-child 'body n))
         (not (ast-bud-node? (ast-child 'elem n)))
         (not ($xsmith_is-hole? (ast-child 'elem n)))
         (λ () (rewrite-subtree (ast-child 'body n)
                                (make-hole 'Block)))))

  #:prop type-info
  [no-return-type
   (λ (n t)
     (define t-1 (if (fuzzing-verifier?) int-type (fresh-type-variable)))
     (define t-2 (if (fuzzing-verifier?) int-type (fresh-type-variable)))
     (hash 'elem t-1
           'elemtwo t-2
           'e (values-type (product-type (list t-1 t-2)))
           'body no-return-type))]
  #:prop render-node-info
  (λ (n)
    `(MethodBlockTwo ,(ast-child 'name (ast-child 'elem n))
                     ,(ast-child 'name (ast-child 'elemtwo n))
                     ,($xsmith_render-node (ast-child 'e n))
                     ,($xsmith_render-node (ast-child 'body n))))]

 [ProcedureApplicationForMethod
  #f
  ([procedure : VariableReference]
   [arguments : Expression * = (create-ast-bud)])
  #:prop edit
  (λ (n)
    (and (ast-bud-node? (ast-child 'arguments n))
         (att-value 'xsmith_no-holes-in-subtree?
                    (ast-child 'procedure n))
         (λ ()
           (rewrite-subtree
            (ast-child 'arguments n)
            (let* ([ft ($xsmith_type (ast-child 'procedure n))]
                   [a-type (product-type #f)]
                   [ft-access (method-type a-type (product-type #f))]
                   [_ (begin (unify! ft ft-access)
                             (force-type-exploration-for-node!
                              (ast-child 'procedure n)))]
                   [arg-types (product-type-inner-type-list a-type)])
              (create-ast-list
               (if (list? arg-types)
                   (map (λ (x) (make-hole 'Expression)) arg-types)
                   (build-list
                    6
                    (λ (x) (make-hole 'Expression))))))))))
  #:prop type-info
  [(values-type (product-type #f))
   (λ (n t)
     (define result-type (fresh-type-variable))
     (unify! t (values-type result-type))
     (define args-node (ast-child 'arguments n))
     (define args-done? (not (ast-bud-node? args-node)))
     (define args (and args-done? (ast-children args-node)))
     (define args-type
       (cond
         [args-done? (product-type (map (λ (x) (fresh-type-variable)) args))]
         [(fuzzing-verifier?)
          (hash-ref! type-table (list 'arg-type-for-ProcedureApplicationForMethod n)
                     (λ () (product-type (for/list ([i (random 4)]) int-type))))]
         [else (product-type #f)]))
     (hash-set
      (if args-done?
          (for/hash ([arg args]
                     [arg-type (product-type-inner-type-list
                                args-type)])
            (values arg arg-type))
          (hash))
      'procedure
      (method-type args-type result-type)))]
  #:prop render-node-info
  (λ (n)
    `(ProcedureApplicationForMethod
      ,($xsmith_render-node (ast-child 'procedure n))
      ,(pretty-print-children (ast-child 'arguments n))))]

 [PrintStmt
  Statement ([es : Expression * = (if (fuzzing-verifier?) 1 (random 1 3))])
  #:prop wont-over-deepen #t
  #:prop type-info [no-return-type
                    (λ (n t)
                      (define children (ast-children (ast-child 'es n)))
                      (for/hash ([c children])
                        (values c (if (fuzzing-verifier?) int-type (fresh-type-variable)))))]
  #:prop render-node-info
  (λ (n) `(PrintStmt ,(append (pretty-print-children (ast-child 'es n))
                              '((StringLiteral "\n")))))
  #:prop io #t]

 [ImmutableSetLiteral
  Expression
  ([expressions : Expression * = ((current-array-length))])
  #:prop wont-over-deepen #t
  #:prop choice-weight (depth-weight)
  #:prop type-info
  [(immutable (fresh-set-type))
   (λ (n t)
     (define et (fresh-type-variable))
     (define at (immutable (set-type et)))
     (subtype-unify! at t)
     (hash 'expressions et))]
  #:prop render-node-info
  (λ (n)
    (match (ast-children (ast-child 'expressions n))
      [(list) `(EmptyImmutableSetLiteral ,(type->string ($xsmith_type n)))]
      [_ `(ImmutableSetLiteral ,(pretty-print-children (ast-child 'expressions n)))]))]

 [ImmutableMultisetLiteral
  Expression
  ([expressions : Expression * = ((current-array-length))])
  #:prop wont-over-deepen #t
  #:prop choice-weight (depth-weight)
  #:prop type-info
  [(immutable (fresh-multiset-type))
   (λ (n t)
     (define et (fresh-type-variable))
     (define at (immutable (multiset-type et)))
     (subtype-unify! at t)
     (hash 'expressions et))]
  #:prop render-node-info
  (λ (n)
    (match (ast-children (ast-child 'expressions n))
      [(list) `(EmptyImmutableMultisetLiteral ,(type->string ($xsmith_type n)))]
      [_ `(ImmutableMultisetLiteral ,(pretty-print-children (ast-child 'expressions n)))]))]

 [StringLiteral
  Expression
  ([v = (random-string/ascii-no-control)])
  #:prop choice-weight (depth-weight)
  #:prop type-info
  [(immutable (array-type char-type))
   (λ (n t) (hash))]
  #:prop render-node-info
  (λ (n) `(StringLiteral ,(ast-child 'v n)))]

 [CharLiteral
  Expression
  ([v = (random-char/ascii-no-control)])
  #:prop choice-weight (depth-weight)
  #:prop type-info
  [char-type
   (λ (n t) (hash))]
  #:prop render-node-info
  (λ (n) `(CharLiteral ,(ast-child 'v n)))]

 #;[ImmutableMapLiteral
  Expression
  ([keys : Expression *]
   [vals : Expression *])
  #:prop wont-over-deepen #t
  ;; These ideally should be reducible, but the keys and values must be reduced together.
  #:prop reducible-list-fields #f
  #:prop choice-weight (depth-weight)
  #:prop fresh
  (let ([elem-count ((current-array-length))])
    (hash 'keys elem-count
          'vals elem-count))
  #:prop type-info
  [(immutable (dictionary-type (fresh-comparable) (map-value-type)))
   (λ (n t)
     (define kt (fresh-type-variable))
     (define vt (fresh-type-variable))
     (define dt (immutable (dictionary-type kt vt)))
     (unify! dt t)
     (hash 'keys kt 'vals vt))]
  #:prop render-node-info
  (λ (n)
    (match (ast-children (ast-child 'keys n))
      [(list) `(EmptyImmutableMapLiteral ,(type->string ($xsmith_type n)))]
      [_ `(ImmutableMapLiteral ,(pretty-print-children (ast-child 'keys n))
                               ,(pretty-print-children (ast-child 'vals n)))]))])


(define-nary-op dafny-comp

  ;;;;;;;;;;
  ;; Char
  ;;;;;;;;;;

  [CharToInt
   #:arity 1
   #:type int-type
   (λ (n t) (list char-type))]

  ;;;;;;;;;;
  ;; Tuple
  ;;;;;;;;;;

  [TupleZeroLiteral
   #:arity 0
   #:type (product0-type)
   (λ (n t) (list))
   #:prop wont-over-deepen #t
   #:prop choice-weight (depth-weight)]

  [TupleTwoLiteral
   #:arity 2
   #:type (product2-type (fresh-type-variable) (fresh-type-variable))
   (λ (n t)
     (define t1 (fresh-type-variable))
     (define t2 (fresh-type-variable))
     (unify! (product2-type t1 t2) t)
     (list t1 t2))
   #:prop wont-over-deepen #t
   #:prop choice-weight (depth-weight)]

  [TupleThreeLiteral
   #:arity 3
   #:type (product3-type (fresh-type-variable) (fresh-type-variable) (fresh-type-variable))
   (λ (n t)
     (define t1 (fresh-type-variable))
     (define t2 (fresh-type-variable))
     (define t3 (fresh-type-variable))
     (unify! (product3-type t1 t2 t3) t)
     (list t1 t2 t3))
   #:prop wont-over-deepen #t
   #:prop choice-weight (depth-weight)]

  [TupleProjFirst
   #:arity 1
   #:type (fresh-type-variable)
   (λ (n t)
     (list (fresh-type-variable (product2-type t (fresh-type-variable))
                                (product3-type t (fresh-type-variable) (fresh-type-variable)))))]

  [TupleProjSecond
   #:arity 1
   #:type (fresh-type-variable)
   (λ (n t)
     (list (fresh-type-variable (product2-type (fresh-type-variable) t)
                                (product3-type (fresh-type-variable) t (fresh-type-variable)))))]

  [TupleProjThird
   #:arity 1
   #:type (fresh-type-variable)
   (λ (n t) (list (product3-type (fresh-type-variable) (fresh-type-variable) t)))]

  ;;;;;;;;;;
  ;; Sequence
  ;;;;;;;;;;

  [SeqLength
   #:arity 1
   #:type int-type
   (λ (n t) (list (immutable (array-type (fresh-type-variable)))))]

  [SeqToMultiset
   #:arity 1
   #:type (immutable (fresh-multiset-type))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (multiset-type t*)))
     (list (immutable (array-type t*))))]

  ;; NOTE: disable until https://github.com/dafny-lang/dafny/issues/1365 is fixed
  #;[SeqDisplay
     #:type (immutable (array-type (fresh-type-variable)))
     (λ (n t)
       (define t* (fresh-type-variable))
       (unify! t (immutable (array-type t*)))
       (hash 'l int-type
             'r (function-type (product-type (list int-type))
                               t*)))]

  [SeqTake
   #:arity 2
   #:type (immutable (array-type (fresh-type-variable)))
   (λ (n t) (list t int-type))]

  [SeqDrop
   #:arity 2
   #:type (immutable (array-type (fresh-type-variable)))
   (λ (n t) (list t int-type))]

  [SeqMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (array-type t))))]

  [SeqNotMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (array-type t))))]

  [SeqSubseq
   #:arity 3
   #:type (immutable (array-type (fresh-type-variable)))
   (λ (n t) (list t int-type int-type))]

  [SeqSliceOneColon
   #:arity 2
   #:type (immutable (array-type (immutable (array-type (fresh-type-variable)))))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (array-type t*)))
     (list t* int-type))]

  [SeqSliceTwo
   #:arity 3
   #:type (immutable (array-type (immutable (array-type (fresh-type-variable)))))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (array-type t*)))
     (list t* int-type int-type))]

  [SeqSliceThree
   #:arity 4
   #:type (immutable (array-type (immutable (array-type (fresh-type-variable)))))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (array-type t*)))
     (list t* int-type int-type int-type))]

  [SeqSliceThreeColon
   #:arity 4
   #:type (immutable (array-type (immutable (array-type (fresh-type-variable)))))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (array-type t*)))
     (list t* int-type int-type int-type))]

  ;;;;;;;;;;
  ;; Set
  ;;;;;;;;;;

  [SetLength
   #:arity 1
   #:type int-type
   (λ (n t) (list (immutable (fresh-set-type))))]

  [SetMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (set-type t))))]

  [SetNotMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (set-type t))))]

  ;;;;;;;;;;
  ;; Multiset
  ;;;;;;;;;;

  [MultisetLength
   #:arity 1
   #:type int-type
   (λ (n t) (list (immutable (fresh-multiset-type))))]

  [MultisetMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (multiset-type t))))]

  [MultisetNotMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (multiset-type t))))]

  [MultisetGet
   #:arity 2
   #:type int-type
   (λ (n _)
     (define t (fresh-comparable))
     (list (immutable (multiset-type t)) t))]

  [MultisetSet
     #:arity 3
     #:type (immutable (fresh-multiset-type))
     (λ (n t)
       (define t* (fresh-type-variable))
       (unify! (immutable (multiset-type t*)) t)
       (list t t* int-type))]

  ;;;;;;;;;;
  ;; Map
  ;;;;;;;;;;

  #;[MapLength
   #:arity 1
   #:type int-type
   (λ (n t) (list (immutable (dictionary-type (fresh-comparable) (map-value-type)))))]

  #;[MapMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (dictionary-type t (map-value-type)))))]

  #;[MapNotMember
   #:arity 2
   #:type bool-type
   (λ (n _)
     (define t (fresh-comparable))
     (list t (immutable (dictionary-type t (map-value-type)))))]

  #;[MapKeys
   #:arity 1
   #:type (immutable (set-type (fresh-comparable)))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (set-type t*)))
     (list (immutable (dictionary-type t* (map-value-type)))))]

  #;[MapValues
   #:arity 1
   #:type (immutable (set-type (fresh-comparable)))
   (λ (n t)
     (define t* (fresh-type-variable))
     (unify! t (immutable (set-type t*)))
     (list (immutable (dictionary-type (fresh-comparable) t*))))])

(define-chain-op dafny-comp
  ;;;;;;;;;;
  ;; Char
  ;;;;;;;;;;

  [CharLessThan
   #:op '("<" "<=" "==")
   #:type bool-type
   (λ (n _) (λ (cn) char-type))]

  [CharGreaterThan
   #:op '(">" ">=" "==")
   #:type bool-type
   (λ (n _) (λ (cn) char-type))]

  ;;;;;;;;;;
  ;; Sequence
  ;;;;;;;;;;

  ;; NOTE: disable until https://github.com/dafny-lang/dafny/issues/1358 is fixed
  #;[SeqPrefix
   #:op '("<" "<=" "==")
   #:type
   bool-type
   (λ (n _)
     (define t (immutable (array-type (fresh-comparable))))
     (hash 'e (λ (cn) t)))]

  [SeqAppend
   #:op '("+")
   #:type
   (immutable (array-type (fresh-type-variable)))
   (λ (n t) (λ (cn) t))]

  ;;;;;;;;;;
  ;; Set
  ;;;;;;;;;;

  [SetSubset
   #:op '("<" "<=" "==")
   #:type bool-type
   (λ (n _)
     (define t (immutable (fresh-set-type)))
     (λ (cn) t))]

  [SetSuperset
   #:op '(">" ">=" "==")
   #:type bool-type
   (λ (n _)
     (define t (immutable (fresh-set-type)))
     (λ (cn) t))]

  [SetDisjoint
   #:op '("!!")
   #:type bool-type
   (λ (n _)
     (define t (immutable (fresh-set-type)))
     (λ (cn) t))]

  [SetUnion
   #:op '("+")
   #:type (immutable (fresh-set-type))
   (λ (n t) (λ (cn) t))]

  [SetIntersect
   #:op '("*")
   #:type (immutable (fresh-set-type))
   (λ (n t) (λ (cn) t))]

  [SetDifference
   #:op '("-")
   #:type (immutable (fresh-set-type))
   (λ (n t) (λ (cn) t))]

  ;;;;;;;;;;
  ;; Multiset
  ;;;;;;;;;;

  [MultisetSubset
   #:op '("<" "<=" "==")
   #:type bool-type
   (λ (n _)
     (define t (immutable (fresh-multiset-type)))
     (λ (cn) t))]

  [MultisetSuperset
   #:op '(">" ">=" "==")
   #:type bool-type
   (λ (n _)
     (define t (immutable (fresh-multiset-type)))
     (λ (cn) t))]

  [MultisetDisjoint
   #:op '("!!")
   #:type bool-type
   (λ (n _)
     (define t (immutable (fresh-multiset-type)))
     (λ (cn) t))]

  [MultisetUnion
   #:op '("+")
   #:type (immutable (fresh-multiset-type))
   (λ (n t) (λ (cn) t))]

  ;; NOTE: disable until https://github.com/dafny-lang/dafny/issues/1360 is fixed
  #;[MultisetIntersect
   #:op '("*")
   #:type
   (immutable (fresh-multiset-type))
   (λ (n t) (λ (cn) t))]

  [MultisetDifference
   #:op '("-")
   #:type (immutable (fresh-multiset-type))
   (λ (n t) (λ (cn) t))]

  ;;;;;;;;;;
  ;; Integer
  ;;;;;;;;;;


  ;; Xsmith already supports Plus, Minus, Times, SafeDivide,
  ;; and it seems not worthwhile to reimplement them here to support chaining
  ;; We do reimplement LessThan and GreaterThan since we want to support chaining
  ;; of it with other operators (like <=, >=, etc.)

  [IntLessThan
   #:op '("<" "<=" "==")
   #:type bool-type
   (λ (n _) (λ (cn) int-type))]

  [IntGreaterThan
   #:op '(">" ">=" "==")
   #:type bool-type
   (λ (n _) (λ (cn) int-type))]

  ;;;;;;;;;;
  ;; Boolean
  ;;;;;;;;;;

  ;; BoolEqual is a part of generic equality

  ;; Xsmith already supports And, Or, Not already, and it seems not worthwhile
  ;; to reimplement them here to support chaining

  [BoolEquiv
   #:op '("<==>")
   #:type bool-type
   (λ (n _) (λ (cn) bool-type))]

  [BoolImplication
   #:op '("==>")
   #:type bool-type
   (λ (n _) (λ (cn) bool-type))]

  [BoolReverseImplication
   #:op '("<==")
   #:type bool-type
   (λ (n _) (λ (cn) bool-type))])

(define (pick . xs)
  (first xs)
  #;(list-ref xs (random (length xs))))

(add-property
 dafny-comp
 lift-type->ast-binder-type
 [#f (λ (type)
       (cond
         [(method-type? type)
          'TopLevelMethodDefinition]
         [(function-type? type)
          (pick 'TopLevelFunMethodDefinition 'Definition)]
         [else 'Definition]))])

(add-property
 dafny-comp
 render-hole-info
 [#f (λ (h) '«HOLE»)])

(add-property
 dafny-comp
 render-node-info

 [Definition (λ (n)
               `(Definition ,(ast-child 'name n) ,($xsmith_render-node (ast-child 'Expression n))))]

 [Block
  (λ (n)
    `(Block ,(pretty-print-children (ast-child 'definitions n))
            ,(pretty-print-children (ast-child 'statements n))))]

 [AssignmentStatement
  (λ (n)
    `(AssignmentStatement ,(ast-child 'name n)
                          ,($xsmith_render-node (ast-child 'Expression n))))]

 [IfElseStatement
  (λ (n)
    `(IfElseStatement
      ,($xsmith_render-node (ast-child 'test n))
      ,($xsmith_render-node (ast-child 'then n))
      ,($xsmith_render-node (ast-child 'else n))))]

 [VariableReference (λ (n) `(VariableReference ,(ast-child 'name n)))]

 [ProcedureApplication
  (λ (n)
    `(ProcedureApplication
      ,($xsmith_render-node (ast-child 'procedure n))
      ,(pretty-print-children (ast-child 'arguments n))))]

 [FormalParameter
  (λ (n)
    `(FormalParameter ,(ast-child 'name n)
                      ,(type->string (ast-child 'type n))))]
 [LambdaWithExpression
  (λ (n)
    `(LambdaWithExpression ,(pretty-print-children (ast-child 'parameters n))
                           ,($xsmith_render-node (ast-child 'body n))))]

 [BoolLiteral (λ (n) `(BoolLiteral ,(ast-child 'v n)))]
 [Not (λ (n) `(Not ,($xsmith_render-node (ast-child 'Expression n))))]
 [And (binary-op-renderer 'And)]
 [Or (binary-op-renderer 'Or)]

 [IntLiteral (λ (n) `(IntLiteral ,(ast-child 'v n)))]
 [Plus (binary-op-renderer 'Plus)]
 [Minus (binary-op-renderer 'Minus)]
 [Times (binary-op-renderer 'Times)]
 [LessThan (binary-op-renderer 'LessThan)]
 [GreaterThan (binary-op-renderer 'GreaterThan)]

 [SafeDivide (binary-op-renderer 'SafeDivide)]

 [ImmutableArrayLiteral
  (λ (n)
    (match (ast-children (ast-child 'expressions n))
      [(list) `(EmptyImmutableArrayLiteral ,(type->string ($xsmith_type n)))]
      [_ `(ImmutableArrayLiteral ,(pretty-print-children (ast-child 'expressions n)))]))]

 [ImmutableArraySafeReference
  (λ (n)
    `(ImmutableArraySafeReference ,($xsmith_render-node (ast-child 'array n))
                                  ,($xsmith_render-node (ast-child 'index n))
                                  ,($xsmith_render-node (ast-child 'fallback n))))]
 [ImmutableArraySafeSet
  (λ (n)
    `(ImmutableArraySafeSet ,($xsmith_render-node (ast-child 'array n))
                            ,($xsmith_render-node (ast-child 'index n))
                            ,($xsmith_render-node (ast-child 'newvalue n))))])

(define (type-thunks-for-concretization)
  (list
   (λ () int-type)
   (λ () bool-type)
   (λ () char-type)
   (λ () (immutable (array-type (fresh-type-variable))))
   (λ () (immutable (set-type (fresh-comparable))))
   (λ () (immutable (multiset-type (fresh-comparable))))
   (λ () (product0-type))
   (λ () (product2-type (fresh-type-variable) (fresh-type-variable)))
   (λ () (product3-type (fresh-type-variable) (fresh-type-variable) (fresh-type-variable)))
   #;(λ () (immutable (dictionary-type (fresh-comparable) (map-value-type))))

   ;; We don't add a function type here because
   ;; xsmith: Got a function type as a type to assign to.  Xsmith's effect tracking requires that assignment can never have a function type.
   #;(λ () (function-type (product-type #f) (fresh-type-variable)))))

(define-xsmith-interface-functions
  [dafny-comp]
  #:fuzzer-name dafny
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithMain
  #:format-render (λ (doc)
                    (cond
                      [(current-dafny-syntax?)
                       (main-print doc)]
                      [else (pretty-format doc #:mode 'write)]))
  #:features ([print-constrained #f])
  #:extra-parameters ([dafny-syntax
                       "As Dafny syntax"
                       current-dafny-syntax?
                       (match-lambda
                         ["true" #t]
                         ["false" #f])])
  #:comment-wrap
  (λ (lines)
    (define commenter
      (cond
        [(current-dafny-syntax?) "//"]
        [else ";;"]))
    (define actual
      (string-join (map (λ (x) (format "~a ~a" commenter x)) lines) "\n"))
    (match (reverse lines)
      [(list _ seed _ ...)
       (string-append (format "// ~a\n" seed) actual)]
      [_ actual])))

(module+ main
  (match (current-command-line-arguments)
    [(vector args ... "--nums" (app string->number n))
     #:when n
     (current-command-line-arguments (list->vector args))
     (for ([i n])
       (dafny-command-line))]
    [_ (dafny-command-line)]))
