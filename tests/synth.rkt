#lang racket

(module+ test
  (require clotho
           rackunit
           "../src/synth.rkt")

  (parameterize ([current-random-source (make-random-source 0)])
    (check-equal? '(Equal (Minus (Minus v (IntLiteral 125))
                                 (Plus (IntLiteral -126) (IntLiteral 125)))
                          (IntLiteral -1))
                  ((synth:pos 123) 'v)))

  (parameterize ([current-random-source (make-random-source 0)])
    (check-equal? '(Equal (Minus (Minus w (IntLiteral 125))
                                 (Plus (IntLiteral -126) (IntLiteral 125)))
                          (IntLiteral -1))
                  ((synth:pos 123) 'w)))

  (parameterize ([current-random-source (make-random-source 42)])
    (check-equal? '(Equal v (Minus (Minus v (IntLiteral 126))
                                   (Minus v (IntLiteral 249))))
                  ((synth:pos 123) 'v)))

  (parameterize ([current-random-source (make-random-source 0)])
    (check-equal? '(Or (Equal (Minus (IntLiteral 41) (IntLiteral 45))
                              (Minus (IntLiteral 40) (IntLiteral 41)))
                       (Equal (IntLiteral 42) v))
                  ((synth:pos 42) 'v))))
