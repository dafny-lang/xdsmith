;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket

(provide main-print)
(require pprint)

(define nest-step 2)

(define (comma-list doc-list)
  (group (nest nest-step (h-append break (h-concat (apply-infix (h-append comma line) doc-list))))))

(define (pair-comma-list doc-list lparen rparen)
  (group
   (h-append lparen
             (nest nest-step
                   (h-append break
                             (h-concat (apply-infix (h-append comma line)
                                                    doc-list))))
             break
             rparen)))

(define (bin-op op l r)
  (h-append lparen
            (pretty l)
            space
            (text op)
            space
            (pretty r)
            rparen))

(define (pretty-chain s op)
  (h-append
   lparen
   (pretty (first s))
   (apply h-append
          (for/list ([s (rest s)]
                     [op op])
            (h-append space (text op) space (pretty s))))
   rparen))

(define (call-with s . es)
  (h-append (text s) (pair-comma-list es lparen rparen)))

(define (multi-line s)
  (v-concat (map text (string-split s "\n"))))

(define (pretty e)
  (match e
    [`(ProgramWithMain ,definitions ,main-body)
     (v-append
      (multi-line #<<EOF
function method safeDivide (a : int, b : int) : int {
  if b == 0 then 0 else a / b
}

function method safeSeqRef<T> (s : seq<T>, i : int, default : T) : T {
  if 0 <= i < |s| then s[i] else default
}

function method safeSeqSet<T> (s : seq<T>, i : int, val : T) : seq<T> {
  if 0 <= i < |s| then s[i := val] else s
}

function method safeSeqTake<T> (s : seq<T>, x : int) : seq<T> {
  if 0 <= x <= |s| then s[..x] else s
}

function method safeSeqDrop<T> (s : seq<T>, x : int) : seq<T> {
  if 0 <= x <= |s| then s[x..] else s
}

function method safeSeqSubseq<T> (s : seq<T>, x : int, y : int) : seq<T> {
  if 0 <= x <= y <= |s| then s[x..y] else s
}

function method safeSeqSlice1Colon<T> (s : seq<T>, x : int) : seq<seq<T>> {
  if 0 <= x <= |s| then s[x:] else [s]
}

function method safeSeqSlice2<T> (s : seq<T>, x : int, y: int) : seq<seq<T>> {
  if 0 <= x && 0 <= y && x + y <= |s| then s[x:y] else [s]
}

function method safeSeqSlice3<T> (s : seq<T>, x : int, y : int, z : int) : seq<seq<T>> {
  if 0 <= x && 0 <= y && 0 <= z && x + y + z <= |s| then s[x:y:z] else [s]
}

function method safeSeqSlice3Colon<T> (s : seq<T>, x : int, y : int, z : int) : seq<seq<T>> {
  if 0 <= x && 0 <= y && 0 <= z && x + y + z <= |s| then s[x:y:z:] else [s]
}

function method lengthNormalize (x : int) : nat {
  (if x < 0 then -x else x) % 50
}

EOF
       )
      (v-concat (map (λ (n) (v-append (pretty n) empty)) definitions))
      (h-append (text "method Main () ")
                (pretty main-body))
      empty)]

    [`(TopLevelMethodDefinition ,name ,meth)
     (h-append (text "method ")
               (text name)
               space
               (pretty meth))]

    [`(TopLevelFunMethodDefinition ,name ,meth)
     (h-append (text "function method ")
               (text name)
               space
               (pretty meth))]

    [`(TopLevelMethod ,params ,rets ,requires ,ensures ,init-rets ,body)
     (v-append
      (h-append
       (pair-comma-list (map pretty params) lparen rparen)
       (nest
        nest-step
        (v-append
         empty
         (h-append (text "returns ") (pair-comma-list (map pretty rets) lparen rparen))
         (h-append (text "requires ") lparen (pretty requires) rparen)
         (h-append (text "ensures ") lparen (pretty ensures) rparen))))
      (h-append
       lbrace
       (nest
        nest-step
        (v-append
         empty
         (v-concat (for/list ([ret rets] [init-ret init-rets])
                     (match-define `(FormalParameter ,name ,_) ret)
                     (pretty `(AssignmentStatement ,name ,init-ret))))
         (pretty body))))
      rbrace)]

    [`(TopLevelFunMethod ,params ,ret-type ,defs ,body)
     (v-append
      (h-append
       (pair-comma-list (map pretty params) lparen rparen)
       (text " : ")
       (text ret-type))

      (h-append lbrace
                (nest nest-step
                      (v-append empty
                                (v-concat (map pretty defs))
                                (pretty body))))
      rbrace)]

    [`(MethodBlock ,x ,e ,body)
     (v-append
      (h-append (text "var ")
                (text x)
                (text " := ")
                (pretty e)
                semi)
      (pretty body))]

    [`(MethodBlockTwo ,x ,y ,e ,body)
     (v-append
      (h-append (text "var ")
                (text x)
                comma
                space
                (text y)
                (text " := ")
                (pretty e)
                semi)
      (pretty body))]

    [`(ProcedureApplicationForMethod ,f ,args)
     (h-append (pretty f) (pair-comma-list (map pretty args) lparen rparen))]

    [`(PrintStmt ,xs)
     (h-append (text "print ") (comma-list (map pretty xs)) (text ";"))]

    [`(SetLength ,s) (h-append (text "|") (pretty s) (text "|"))]
    [`(MultisetLength ,s) (h-append (text "|") (pretty s) (text "|"))]


    [`(,(or 'SeqPrefix 'SeqAppend
            'SetSubset 'SetSuperset 'SetDisjoint 'SetUnion 'SetDifference 'SetIntersect
            'MultisetSubset 'MultisetSuperset 'MultisetDisjoint 'MultisetUnion 'MultisetDifference 'MultisetIntersect
            'IntLessThan 'IntGreaterThan
            'BoolEquiv 'BoolImplication 'BoolReverseImplication
            'CharLessThan 'CharGreaterThan) ,s ,op)
     (pretty-chain s op)]

    [`(Definition ,x ,e)
     (h-append (text "var ") (text x) (text " := ") (pretty e) semi)]

    [`(Block ,definitions ,statements)
     (h-append
      lbrace
      (nest nest-step
            (h-append
             line
             (v-concat
              (append
               (map pretty definitions)
               (map pretty statements)))))
      line
      rbrace)]

    [`(AssignmentStatement ,x ,e) (h-append (text x) (text " := ") (pretty e) semi)]

    [`(IfElseStatement ,c ,t ,e)
     (h-append
      (h-append (text "if ") lparen (pretty c) rparen space)
      (pretty t)
      (text " else ")
      (pretty e))]

    [`(VariableReference ,x) (text x)]

    [`(ProcedureApplication ,f ,args)
     (h-append (pretty f) (pair-comma-list (map pretty args) lparen rparen))]

    [`(FormalParameter ,x ,t)
     (h-append (text x)
               (text " : ")
               (text t))]

    [`(LambdaWithExpression ,params ,body)
     (h-append lparen
               (pair-comma-list (map pretty params) lparen rparen)
               (text " => ")
               (pretty body)
               rparen)]

    [`(BoolLiteral ,b) (text (if b "true" "false"))]

    [`(Not ,e) (h-append (text "!") lparen (pretty e) rparen)]

    [`(IntLiteral ,n) (text (number->string n))]

    [`(And ,l ,r) (bin-op "&&" l r)]
    [`(Or ,l ,r) (bin-op "||" l r)]
    [`(Plus ,l ,r) (bin-op "+" l r)]
    [`(Minus ,l ,r) (bin-op "-" l r)]
    [`(Times ,l ,r) (bin-op "*" l r)]
    [`(LessThan ,l ,r) (bin-op "<" l r)]
    [`(GreaterThan ,l ,r) (bin-op ">" l r)]

    [`(SafeDivide ,l ,r)
     (h-append (text "safeDivide")
               lparen
               (pretty l)
               (text ", ")
               (pretty r)
               rparen)]

    [`(EmptyImmutableSetLiteral ,t)
     (text (format "(var tmpData : ~a := {}; tmpData)" t))]
    [`(ImmutableSetLiteral ,xs)
     (pair-comma-list (map pretty xs) lbrace rbrace)]

    [`(EmptyImmutableMultisetLiteral ,t)
     (text (format "(var tmpData : ~a := multiset{}; tmpData)" t))]
    [`(ImmutableMultisetLiteral ,xs)
     (h-append (text "multiset") (pair-comma-list (map pretty xs) lbrace rbrace))]

    [`(SetMember ,l ,r) (bin-op "in" l r)]
    [`(SetNotMember ,l ,r) (bin-op "!in" l r)]

    [`(MultisetMember ,l ,r) (bin-op "in" l r)]
    [`(MultisetNotMember ,l ,r) (bin-op "!in" l r)]

    [`(MultisetGet ,l ,r)
     ;; NOTE: "as int" to avoid type mismatch between int and nat
     (h-append lparen
               (pretty l)
               lbracket
               (pretty r)
               rbracket
               (text " as int")
               rparen)]
    [`(MultisetSet ,collection ,idx ,val)
     ;; NOTE: call lengthNormalize since we can't set it to negative number
     ;; and setting to a very large number will crash printing
     (h-append lparen
               (pretty collection)
               lbracket
               (pretty idx)
               (text " := ")
               (call-with "lengthNormalize" (pretty val))
               rbracket
               rparen)]

    [`(Assert ,e) (h-append (text "assert ") (pretty e) semi)]
    [`(Equal ,l ,r) (bin-op "==" l r)]

    ;; Sequence

    [`(SeqLength ,s) (h-append (text "|") (pretty s) (text "|"))]
    [`(SeqMember ,l ,r) (bin-op "in" l r)]
    [`(SeqNotMember ,l ,r) (bin-op "!in" l r)]
    [`(SeqDisplay ,l ,r)
     (call-with "seq"
                (call-with "lengthNormalize" (pretty l))
                (pretty r))]
    [`(SeqTake ,l ,r) (call-with "safeSeqTake" (pretty l) (pretty r))]
    [`(SeqDrop ,l ,r) (call-with "safeSeqDrop" (pretty l) (pretty r))]
    [`(SeqSubseq ,s ,x ,y) (call-with "safeSeqSubseq" (pretty s) (pretty x) (pretty y))]
    [`(SeqToMultiset ,e) (call-with "multiset" (pretty e))]
    [`(SeqSliceOneColon ,e ,a) (call-with "safeSeqSlice1Colon" (pretty e) (pretty a))]
    [`(SeqSliceTwo ,e ,a ,b) (call-with "safeSeqSlice2" (pretty e) (pretty a) (pretty b))]
    [`(SeqSliceThree ,e ,a ,b ,c) (call-with "safeSeqSlice3" (pretty e) (pretty a) (pretty b) (pretty c))]
    [`(SeqSliceThreeColon ,e ,a ,b ,c) (call-with "safeSeqSlice3Colon" (pretty e) (pretty a) (pretty b) (pretty c))]

    [`(EmptyImmutableArrayLiteral ,t)
     (text (format "(var tmpData : ~a := []; tmpData)" t))]
    [`(ImmutableArrayLiteral ,xs)
     (pair-comma-list (map pretty xs) lbracket rbracket)]
    [`(ImmutableArraySafeReference ,arr ,idx ,fallback)
     (call-with "safeSeqRef"
                (pretty arr)
                (pretty idx)
                (pretty fallback))]
    [`(ImmutableArraySafeSet ,arr ,idx ,val)
     (call-with "safeSeqSet"
                (pretty arr)
                (pretty idx)
                (pretty val))]

    ;; Map
    [`(EmptyImmutableMapLiteral ,t)
     (text (format "(var tmpData : ~a := map[]; tmpData)" t))]
    [`(ImmutableMapLiteral ,keys ,vals)
     (h-append (text "map")
               (pair-comma-list
                (for/list ([key keys] [val vals])
                  (h-append (pretty key) (text " := ") (pretty val)))
                lbracket
                rbracket))]
    [`(MapLength ,s) (h-append (text "|") (pretty s) (text "|"))]
    [`(MapMember ,l ,r) (bin-op "in" l r)]
    [`(MapNotMember ,l ,r) (bin-op "!in" l r)]
    [`(MapKeys ,e) (h-append (pretty e) (text ".Keys"))]
    [`(MapValues ,e) (h-append (pretty e) (text ".Values"))]

    ;; Tuple

    [`(,(or 'TupleZeroLiteral 'TupleTwoLiteral 'TupleThreeLiteral) ,@xs)
     (pair-comma-list (map pretty xs) lparen rparen)]
    [`(TupleProjFirst ,x)
     (h-append (pretty x) (text ".0"))]
    [`(TupleProjSecond ,x)
     (h-append (pretty x) (text ".1"))]
    [`(TupleProjThird ,x)
     (h-append (pretty x) (text ".2"))]

    ;; String
    [`(StringLiteral ,s) (text (format "~v" s))]

    ;; Char
    [`(CharLiteral ,s) (text (match s
                               [#\' "'\\''"]
                               [_ (format "'~a'" s)]))]
    [`(CharToInt ,e) (h-append lparen (pretty e) (text " as int") rparen)]))

(define (main-print datum)
  (pretty-format (pretty datum)))
