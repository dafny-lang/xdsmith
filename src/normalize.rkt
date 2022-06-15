#lang racket

(provide normalize-cs
         normalize-go
         normalize-java
         normalize-js)

(define readtable/fundamental
  (make-readtable
   #f

   ;; don't treat ; as comment
   #\; #\a #f

   #\" #\a #f
   #\' #\a #f
   #\| #\a #f

   #\,
   'terminating-macro
   (λ (ch port src line col pos) ", ")

   #\newline
   'terminating-macro
   (λ (ch port src line col pos) ch)

   ;; don't make +i become 0+1i
   #\+ #\a #f
   #\- #\a #f))

(define (split-by xs e)
  (for/fold ([result '()] [top '()] #:result (reverse (cons (reverse top) result)))
            ([x (in-list xs)])
    (cond
      [(equal? e x)
       (values (cons (reverse top) result) '())]
      [else
       (values result (cons x top))])))

(define readtable/base
  (make-readtable
   readtable/fundamental

   #\{
   'terminating-macro
   (λ (ch port src line col pos)
     (define xs (map ~a (read/recursive port ch readtable/fundamental)))
     (define all-elems (map string-append* (split-by xs ", ")))
     (~a "{" (string-join (sort all-elems string<?) ", ") "}"))

   #\[
   'terminating-macro
   (λ (ch port src line col pos)
     (define xs (map ~a (read/recursive port ch readtable/fundamental)))
     (~a "[" (string-join xs "") "]"))

   #\M
   'dispatch-macro
   (λ (ch port src line col pos)
     (~a "multiset" (read port)))

   #\D
   'dispatch-macro
   (λ (ch port src line col pos)
     (define xs (map ~a (read/recursive port #f readtable/fundamental)))
     (define xs-norm
       (match (split-by xs ", ")
         ['(()) '(())]
         [xs
          (for/fold ([acc '()] [seen (set)] #:result (reverse acc)) ([x (in-list (reverse xs))])
            (cond
              [(set-member? seen (first x))
               (values acc seen)]
              [else
               (values (cons x acc) (set-add seen (first x)))]))]))
     (define all-elems (map string-append* xs-norm))
     (~a "map[" (string-join (sort all-elems string<?) ", ") "]"))))

(define readtable/java
  readtable/base)

(define readtable/cs
  (make-readtable
   readtable/base
   #\F
   'dispatch-macro
   (λ (ch port src line col pos)
     (read port)
     "Function")))

(define readtable/js
  (make-readtable
   readtable/base
   #\F
   'dispatch-macro
   (λ (ch port src line col pos)
     (parameterize ([current-readtable readtable/fundamental])
       (read port)
       (read port))
     "Function")))

(define (peek/equal? str in)
  (equal? str (peek-string (string-length str) 0 in)))

(define (peek/read? str in)
  (and (peek/equal? str in)
       (read-string (string-length str) in)))

(define readtable/go
  (make-readtable
   readtable/base
   #\F
   'dispatch-macro
   (λ (ch port src line col pos)
     ;; read argument types
     (read port)
     ;; read return type
     (cond
       ;; two spaces; one exists already; another because we insert it ourselves
       [(peek/equal? "  #F" port) (read port)]
       [(or (peek/read? " dafny.Seq" port)
            (peek/read? " dafny.Int" port)
            (peek/read? " dafny.Char" port)
            (peek/read? " dafny.Set" port)
            (peek/read? " dafny.MultiSet" port)
            (peek/read? " dafny.Map" port)
            (peek/read? " dafny.Tuple" port)
            (peek/read? " bool" port))
        (void)]
       [else (error 'go-readtable "Unexpected ~s" (read-string 100 port))])
     "Function")))

(define (deal-with-common s)
  ;; normalize multiset and map
  (regexp-replace* #px"multiset\\{" (regexp-replace* #px"map\\[" s " #D[") " #M{"))

(define (normalize-java s)
  (define p (open-input-string
             (deal-with-common
              (regexp-replace* #px"_System\\.__default\\$\\$Lambda\\$[a-z0-9/@]*" s "Function"))))

  (string-join (map ~a
                    (parameterize ([current-readtable readtable/java])
                      (sequence->list (in-port read p))))
               ""))

(define (normalize-cs s)
  (define p (open-input-string
             (deal-with-common
              (regexp-replace* #px"System\\.Func`\\d+" s " #F"))))
  (string-join (map ~a
                    (parameterize ([current-readtable readtable/cs])
                      (sequence->list (in-port read p))))
               ""))

(define (normalize-js s)
  (define p (open-input-string
             (deal-with-common
              (regexp-replace* #px"(function |lift__\\d+(?=\\())" s " #F"))))
  (string-join (map ~a
                    (parameterize ([current-readtable readtable/js])
                      (sequence->list (in-port read p))))
               ""))

(define (normalize-go s)
  (define p (open-input-string
             (deal-with-common
              (regexp-replace* #px"func(?=\\()" s " #F"))))
  (string-join (map ~a
                    (parameterize ([current-readtable readtable/go])
                      (sequence->list (in-port read p))))
               ""))
