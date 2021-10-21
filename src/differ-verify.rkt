;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang clotho

(require syntax/location
         "oracle.rkt"
         "pretty.rkt"
         "basic.rkt"
         racket/string
         racket/port
         racket/match
         racket/list
         racket/cmdline
         racket/sequence
         racket/file
         (prefix-in synth: "synth.rkt"))

(module gen racket
  (require "fuzzer.rkt")
  (provide gen)
  (define (gen seed)
    (define s
      (with-output-to-string
        (λ () (parameterize ([current-command-line-arguments
                              (vector-append
                               #("--with-print-constrained" "true")
                               (if seed
                                   (vector "--seed" seed)
                                   #()))])
                (dafny-command-line)))))
    (define p (open-input-string s))
    (define seed-info (read-line p))
    (define datum (read p))
    (values seed-info datum p)))

(define path-to-gen (quote-module-path gen))

(define current-mode (make-parameter #f))
(define current-section (make-parameter #f))
(define current-data (make-parameter #f))
(define current-need-dup (make-parameter #f))
(define current-meth-name (make-parameter #f))
(define current-negative (make-parameter #f))

(define memo-table (make-hash))

(define (synth:pos n)
  (cond
    [(<= (length (hash-ref memo-table n '())) 5)
     (define new (synth:synth:pos n))
     (hash-update! memo-table n (λ (old) (cons new old)) '())
     new]
    [else (random-ref (hash-ref memo-table n))]))

(define (synth:neg n)
  (synth:synth:neg n))

(define (process e)
  (define (loop e)
    (match e
      [`(ProgramWithMain ,definitions ,main-body)
       (parameterize ([current-need-dup (make-hash)])
         (define main-body* (loop main-body))
         (let loop-defs ([definitions (reverse definitions)] [result '()])
           (match definitions
             ['() `(ProgramWithMain ,result ,main-body*)]
             [(cons (and def `(TopLevelMethodDefinition ,name ,meth)) definitions)
              (define definitions* (for/list ([name* (hash-ref (current-need-dup) name '())])
                                     `(TopLevelMethodDefinition ,name* ,meth)))
              (match definitions*
                ['() ; new ones
                 (loop-defs definitions (cons (loop def) result))]
                [_ (loop-defs (append definitions* definitions) result)])]
             [(cons def definitions)
              (loop-defs definitions (cons (loop def) result))])))]

      [`(TopLevelMethodDefinition ,name ,meth)
       `(TopLevelMethodDefinition ,name ,(parameterize ([current-meth-name name]) (loop meth)))]

      [`(TopLevelFunMethodDefinition ,name ,meth)
       `(TopLevelFunMethodDefinition ,name ,(loop meth))]

      [`(TopLevelMethod ,params ,rets ,pre-dummy ,post-dummy ,init-rets ,body)
       (match (current-mode)
         ['preprocess
          (define (gen-params mode params)
            (for/list ([raw-param params])
              (match-define `(FormalParameter ,param ,_) raw-param)
              `(PrintStmt ((StringLiteral ,(format "(~a-for ~a ~a " mode (current-meth-name) param))
                           (VariableReference ,param)
                           (StringLiteral ")\n")))))
          `(TopLevelMethod
            ,(map loop params)
            ,(map loop rets)
            ,pre-dummy
            ,post-dummy
            ,init-rets
            (Block ()
                   ,(append (gen-params 'params params)
                            (list `(PrintStmt ((StringLiteral ,(format "(meth-for ~a)\n" (current-meth-name))))))
                            (list (loop body))
                            (gen-params 'rets rets))))]
         ['postprocess
          (define (gen-params data params meth-data)
            (cond
              [(hash-has-key? meth-data (current-meth-name))
               (for/fold ([form '(BoolLiteral #t)]) ([raw-param params])
                 (match-define `(FormalParameter ,param ,_) raw-param)
                 (cond
                   [(hash-has-key? data (list (current-meth-name) param))
                    `(And (Equal (VariableReference ,param)
                                 (IntLiteral ,(hash-ref data (list (current-meth-name) param))))
                          ,form)]
                   [else '(BoolLiteral #f)]))]
              [else '(BoolLiteral #f)]))

          (match-define (list _ arg-data ret-data meth-data) (current-data))
          `(TopLevelMethod ,(map loop params)
                           ,(map loop rets)
                           ,(gen-params arg-data params meth-data)
                           ,(gen-params ret-data rets meth-data)
                           ,init-rets
                           ,(loop body))])]

      [`(TopLevelFunMethod ,params ,ret-type ,defs ,body)
       `(TopLevelFunMethod ,(map loop params) ,ret-type ,(map loop defs) ,(loop body))]

      [`(MethodBlock ,x ,e ,body)
       `(MethodBlock ,x ,(loop e) ,(loop body))]

      [`(MethodBlockTwo ,x ,y ,e ,body)
       `(MethodBlockTwo ,x ,y ,(loop e) ,(loop body))]

      [`(ProcedureApplicationForMethod (VariableReference ,f) ,args)
       (define new-name (format "~a_~a" f (length (hash-ref (current-need-dup) f '()))))
       (hash-update! (current-need-dup) f (λ (old) (cons new-name old)) '())
       `(ProcedureApplicationForMethod (VariableReference ,new-name) ,(map loop args))]

      [`(PrintStmt ,xs)
       (match (current-mode)
         ['preprocess
          (define this-section (current-section))
          (current-section (add1 this-section))
          `(PrintStmt ,(append (list `(StringLiteral ,(format "(section ~a " this-section)))
                               (map loop xs)
                               (list '(StringLiteral ")\n"))))]
         ['postprocess
          (define this-section (current-section))
          (current-section (add1 this-section))
          (match-define (list section-data _ _ _) (current-data))
          (cond
            [(hash-has-key? section-data this-section)
             (define synth
               (if (= this-section (current-negative))
                   synth:neg
                   synth:pos))
             (define s ((synth (hash-ref section-data this-section))
                        (first (map loop xs))))
             `(Assert ,s)]
            [else '(Assert (BoolLiteral #f))])])]


      [`(Definition ,x ,e)
       `(Definition ,x ,(loop e))]

      [`(Block ,definitions ,statements)
       `(Block ,(map loop definitions) ,(map loop statements))]

      [`(AssignmentStatement ,x ,e)
       `(AssignmentStatement ,x ,(loop e))]

      [`(ProcedureApplication ,f ,args) `(ProcedureApplication ,(loop f) ,(map loop args))]

      [`(LambdaWithExpression ,params ,body)
       `(LambdaWithExpression ,(map loop params) ,(loop body))]

      ;; literals
      [`(,(and op (or 'IntLiteral 'BoolLiteral 'StringLiteral 'CharLiteral
                      'EmptyImmutableSetLiteral
                      'EmptyImmutableMultisetLiteral
                      'EmptyImmutableArrayLiteral
                      'EmptyImmutableMapLiteral
                      'VariableReference
                      'FormalParameter)) ,xs ...) `(,op ,@xs)]

      ;; nary operators
      [(cons
        (and op
             (or 'Not 'And 'Or
                 'Plus 'Minus 'Times
                 'SafeDivide
                 'LessThan 'GreaterThan
                 'SetLength 'SetMember 'SetNotMember
                 'MultisetLength 'MultisetMember 'MultisetNotMember 'MultisetGet 'MultisetSet
                 'SeqLength 'SeqToMultiset 'SeqMember 'SeqNotMember 'SeqDisplay 'SeqTake 'SeqDrop 'SeqSubseq 'SeqSliceOneColon 'SeqSliceTwo 'SeqSliceThree 'SeqSliceThreeColon
                 'MapLength 'MapMember 'MapNotMember 'MapKeys 'MapValues
                 'ImmutableArraySafeSet 'ImmutableArraySafeReference
                 'TupleZeroLiteral 'TupleTwoLiteral 'TupleThreeLiteral 'TupleProjFirst 'TupleProjSecond 'TupleProjThird
                 'IfElseStatement))
             xs)
       (cons op (map loop xs))]

      ;; list of expressions
      [`(,(and op (or 'ImmutableSetLiteral
                      'ImmutableMultisetLiteral
                      'ImmutableArrayLiteral)) ,xs)
       `(,op ,(map loop xs))]

      [`(ImmutableMapLiteral ,xs ,ys)
       `(ImmutableMapLiteral ,(map loop xs) ,(map loop ys))]

      ;; chaining operators
      [`(,(and op (or 'SeqAppend 'SeqPrefix
                      'SetSubset 'SetSuperset 'SetDisjoint 'SetUnion 'SetIntersect 'SetDifference
                      'MultisetSubset 'MultisetSuperset 'MultisetDisjoint 'MultisetUnion 'MultisetIntersect 'MultisetDifference
                      'IntLessThan 'IntGreaterThan
                      'BoolEquiv 'BoolImplication 'BoolReverseImplication)) ,xs ,ops)
       `(,op ,(map loop xs) ,ops)]))
  (parameterize ([current-section 0])
    (loop e)))

(define (build-data xs)
  (define section-data (make-hash))
  (define param-data (make-hash))
  (define ret-data (make-hash))
  (define meth-data (make-hash))
  (for ([x xs])
    (match x
      [`(section ,n ,v) (hash-set! section-data n v)]
      [`(params-for ,meth ,arg ,val) (hash-set! param-data (list (symbol->string meth) (symbol->string arg)) val)]
      [`(rets-for ,meth ,arg ,val) (hash-set! ret-data (list (symbol->string meth) (symbol->string arg)) val)]
      [`(meth-for ,meth) (hash-set! meth-data (symbol->string meth) #t)]))
  (list section-data param-data ret-data meth-data))

(define (handling-error e)
  (string-contains? (xdsmith:bad-compilation-out e)
                    "Error: assertion violation"))

(module+ main
  (define current-limit +inf.0)
  (define current-batch-size 5)
  (define current-print-pre #f)
  (define current-print-post #f)
  (define current-expect-failure? #f)
  (command-line
   #:once-each
   [("--print-pre") "Print preprocessed code"
                    (set! current-print-pre #t)]
   [("--print-post") "Print postprocessed code"
                     (set! current-print-post #t)]
   [("--limit") num-limit "Iteration limit (default: none)"
                (set! current-limit (string->number num-limit))]
   [("--size") batch-size "Batch size (default: 10)"
               (set! current-batch-size (string->number batch-size))]
   [("--negative") "Expect a verification failure"
                   (set! current-expect-failure? #t)]
   #:args ([seed #f])
   (define path "tmp.dfy")

   (define (main seed gen)
     (define-values (seed-info datum port) (gen seed))
     (displayln seed-info)
     (match (regexp-match #px"\\d+$" seed-info)
       [(list seed)
        (parameterize ([current-random-source (make-random-source (string->number seed))])
          (define datum*
            (parameterize ([current-mode 'preprocess])
              (process datum)))
          (with-output-to-file path #:exists 'replace
            (λ ()
              (displayln seed-info)
              (display (main-print datum*))))
          (when current-print-pre
            (displayln (file->string path)))
          (match (get-result path (λ (e) #f))
            ['known-mismatch (void)]
            [s
             (define xs (sequence->list (in-port read (open-input-string s))))
             (define the-data (build-data xs))
             (match (hash-keys (first the-data))
               ['() (void)]
               [xs
                (define datum**
                  (parameterize ([current-mode 'postprocess]
                                 [current-data the-data]
                                 [current-negative (if current-expect-failure?
                                                       (random-ref xs)
                                                       -1)])
                    (process datum)))
                (with-output-to-file path #:exists 'replace
                  (λ ()
                    (displayln seed-info)
                    (display (main-print datum**))))
                (when current-print-post
                  (displayln (file->string path)))
                (cond
                  [current-expect-failure?
                   (match (get-result path handling-error)
                     ['error (void)]
                     [v (raise-user-error 'differ-verifier "Expected a failure, got: ~v" v)])]
                  [else
                   (match (get-result path (λ (_) #f))
                     [(? string?) (void)]
                     [v (raise-user-error 'differ-verifier "Expected an ok, got: ~v " v)])])])]))]
       [_ (error (port->string port))]))

   (cond
     [seed (main seed (dynamic-require path-to-gen 'gen))]
     [else
      (for ([i (in-range current-limit)])
        (when (> (hash-count memo-table) 10000)
          (hash-clear! memo-table))
        (parameterize ([current-namespace (make-base-namespace)])
          (define gen (dynamic-require path-to-gen 'gen))
          (for ([j (in-range current-batch-size)])
            (main #f gen))))])))
