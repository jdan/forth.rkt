#lang racket

(require rackunit)

(define (binop->forth builtin)
  (λ (out)
    (let ([a (car out)]
          [b (cadr out)]
          [rest (cddr out)])
      (cons (apply builtin (list a b)) rest))))

(define stdlib
  (list (list '+ (binop->forth +))))

(define (step in out)
  (cond [(null? in) out]
        [(number? (car in)) (cons (car in) out)]
        [else
         (let ([fn (assoc (car in) stdlib)])
           (if fn
               ((cadr fn) out)
               (error "Undefined operation --" (car in))))]))

(define (eval-with-stack in out)
  (if (null? in)
      out
      (eval-with-stack (cdr in)
                       (step in out))))

(define (eval in)
  (eval-with-stack in '()))

(check-equal? (eval '(4)) '(4))
(check-equal? (eval '(4 5 6)) '(6 5 4))
(check-equal? (eval '(4 5 6 +)) '(11 4))
(check-equal? (eval '(4 5 6 + +)) '(15))

(check-exn exn:fail? (λ () (eval '(4 5 6 + + unknown))))