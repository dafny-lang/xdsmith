#lang racket

(module+ test
  (require rackunit
           xdsmith/pretty)
  (check-equal?
   (main-print
    '(ProcedureApplication (VariableReference "f")
                           ((VariableReference "a")
                            (VariableReference "b")
                            (VariableReference "c"))))
   "f(a, b, c)")

  (check-equal?
   (main-print
    '(ProcedureApplication (VariableReference "ffffffffffffffffffff")
                           ((VariableReference "aaaaaaaaaaaaaaaaaaaa")
                            (VariableReference "bbbbbbbbbbbbbbbbbbbb")
                            (VariableReference "cccccccccccccccccccc"))))
   #<<EOF
ffffffffffffffffffff(
  aaaaaaaaaaaaaaaaaaaa,
  bbbbbbbbbbbbbbbbbbbb,
  cccccccccccccccccccc
)
EOF
   ))
