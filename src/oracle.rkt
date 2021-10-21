;; Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
;; SPDX-License-Identifier: MIT

#lang racket

(provide oracle get-result)
(require "basic.rkt")

(define dafny-path (or (getenv "DAFNYPATH") "/workspace/dafny/Binaries/Dafny"))

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

(struct ans (out name) #:transparent)

(define (extract-ans #:norm normalizer #:name name path target . other-args)
  (define-values (in-for-out out-port) (make-pipe))
  (define-values (in-for-err err-port) (make-pipe))
  (define exit-ok?
    (parameterize ([current-output-port out-port]
                   [current-error-port err-port])
      (begin0 (apply system* dafny-path path (format "/compileTarget:~a" target) "/compile:3" other-args)
        (close-output-port out-port)
        (close-output-port err-port))))
  (define out-str (port->string in-for-out))
  (define err-str (port->string in-for-err))
  (unless exit-ok?
    (raise (xdsmith:bad-compilation target out-str err-str)))
  (match (regexp-match #px"^(.*?)Running\\.\\.\\.\n\n(.*)$" out-str)
    [(list _ _ the-ans) (ans (normalizer the-ans) name)]
    [_ (raise (xdsmith:unexpected-output target out-str err-str))]))

(define (known-mismatch)
  (displayln "Known mismatch or error!")
  'known-mismatch)

(define ((handler expect?) e)
  (match e
    [(? expect?) 'error]
    [(xdsmith:bad-compilation target out err)
     (displayln (format "Compilation error for ~a" target))
     (displayln "Output:")
     (displayln out)
     (displayln "Error:")
     (displayln err)
     (newline)
     (cond
       [(string-contains? (xdsmith:bad-compilation-err e)
                          "Process terminated. Assumption failed.")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "error CS0305: Using the generic type 'Func<TResult>' requires 1 type arguments")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "Error: the type of this expression is underspecified")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "error: local variables referenced from a lambda expression must be final or effectively final")
        (known-mismatch)]
       [(string-contains? (xdsmith:bad-compilation-out e)
                          "Error: All elements of display must have some common supertype")
        (known-mismatch)]
       [else (displayln "New result!")
             (exit 1)])]
    [(xdsmith:unexpected-output target out err)
     (displayln (format "Unexpected output for ~a" target))
     (displayln "Output:")
     (displayln out)
     (displayln "Error:")
     (displayln err)
     (newline)
     (exit 1)]
    [_ (raise e)]))

(define (display-output s)
  (match (current-print-style)
    ['blob (printf "~v" s)]
    ['readable
     (define sep "******************")
     (displayln (~a sep "\n" s "\n" sep))]))

(define (check-pair a b)
  (unless (or (current-check-all?) (equal? (ans-out a) (ans-out b)))
    (printf "~a:\n" (ans-name a))
    (display-output (ans-out a))
    (newline)
    (printf "~a:\n" (ans-name b))
    (display-output (ans-out b))
    (newline)
    (raise-user-error "Mismatch!")))

(define (check-all . xs)
  (define outs (map ans-out xs))
  (unless (for/and ([out (rest outs)])
            (equal? (first outs) out))
    (for ([ans xs])
      (printf "~a:\n" (ans-name ans))
      (display-output (ans-out ans))
      (newline))
    (raise-user-error "Mismatch!")))

(define (oracle path)
  (with-handlers ([xdsmith-error? (handler (λ (_) #f))])
    (define ans-js (extract-ans path "js" #:norm normalize-js #:name "JS"))
    (define ans-go (extract-ans path "go" #:norm normalize-go #:name "Go"))
    (check-pair ans-js ans-go)
    (define ans-cs (extract-ans path "cs" #:norm normalize-cs #:name "C#"))
    (check-pair ans-js ans-cs)
    (define ans-java (extract-ans path "java" #:norm normalize-java #:name "Java 11"))
    (check-pair ans-js ans-java)
    (define ans-cs* (extract-ans path "cs" "/optimize" #:norm normalize-cs #:name "C# (optimized)"))
    (check-pair ans-js ans-cs*)
    (when (current-check-all?)
      (check-all ans-js ans-go ans-cs ans-java ans-cs*))))

(define (get-result path handling-error)
  (with-handlers ([xdsmith-error? (handler handling-error)])
    (extract-ans path "js" #:norm values)))
