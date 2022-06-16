#lang racket

(module+ test
  (require clotho
           rackunit
           xdsmith/synth)

  (parameterize ([current-random-source (make-random-source 0)])
    (check-equal? ((synth:pos 123) 'v)
                  '(Equal (Minus (Minus v (IntLiteral 125))
                                 (Plus (IntLiteral -126) (IntLiteral 125)))
                          (IntLiteral -1)))

    (check-equal? ((synth:neg 123) 'v)
                  '(Or (Equal (Plus (IntLiteral -124) v)
                              (Plus (IntLiteral -125) v))
                       (Equal (Minus (IntLiteral -124) (IntLiteral 123))
                              (Minus (IntLiteral -370) v)))))

  (parameterize ([current-random-source (make-random-source 0)])
    (check-equal? ((synth:pos 123) 'w)
                  '(Equal (Minus (Minus w (IntLiteral 125))
                                 (Plus (IntLiteral -126) (IntLiteral 125)))
                          (IntLiteral -1)))

    (check-equal? ((synth:neg 123) 'w)
                  '(Or (Equal (Plus (IntLiteral -124) w)
                              (Plus (IntLiteral -125) w))
                       (Equal (Minus (IntLiteral -124) (IntLiteral 123))
                              (Minus (IntLiteral -370) w)))))

  (parameterize ([current-random-source (make-random-source 42)])
    (check-equal? ((synth:pos 123) 'v)
                  '(Equal v (Minus (Minus v (IntLiteral 126))
                                   (Minus v (IntLiteral 249)))))

    (check-equal? ((synth:neg 123) 'v)
                  '(Equal (Plus v (Plus v (IntLiteral -246)))
                          (IntLiteral -246))))

  (parameterize ([current-random-source (make-random-source 0)])
    (check-equal? ((synth:pos 42) 'v)
                  '(Or (Equal (Minus (IntLiteral 41) (IntLiteral 45))
                              (Minus (IntLiteral 40) (IntLiteral 41)))
                       (Equal (IntLiteral 42) v)))

    (check-equal? ((synth:neg 42) 'v)
                  '(Or (And (LessThan v v)
                            (LessThan (IntLiteral -45) v))
                       (LessThan (Plus (IntLiteral -43) v)
                                 (Plus (IntLiteral -43) (IntLiteral 42)))))))
