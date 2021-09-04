#lang racket

(require rackunit)

(define (binop->forth builtin)
  (λ (in out)
    (let ([a (car out)]
          [b (cadr out)]
          [rest (cddr out)])
      (values in
              (cons (apply builtin (list a b)) rest)))))

(define stdlib
  (list (list 'dup (λ (in out) (values in
                                       (cons (car out) out))))
        (list 'call (λ (in out)
                      (values (append (car out) in)
                              (cdr out))))
        (list '+ (binop->forth +))
        (list '* (binop->forth *))))

(define make-env list)
(define env-input-stack car)
(define env-output-stack cadr)
(define env-defs caddr)

(define (step env)
  (define (definition? symb)
    (eq? symb ':))
  (define (end-definition? symb)
    (eq? symb '\;))

  (let* ([in (env-input-stack env)]
         [out (env-output-stack env)]
         [defs (env-defs env)])
  
    (cond [(null? in) (make-env in out defs)]

          ; Literals
          [(or (number? (car in))
               (pair? (car in)))
           (make-env (cdr in)
                     (cons (car in) out)
                     defs)]

          ; Definitions
          [(definition? (car in))
           (let-values ([; IN  => '(x dup ; hello world)
                         ; OUT => (values '(x dup) '(\; hello world))
                         (definition rest)
                         (splitf-at (cdr in)
                                    (λ (symb)
                                      (not (end-definition? symb))))])
             
             (make-env (cdr rest) ; Trim the `;` from the definition end
                       out
                       ; Add the definition to our assoc
                       (cons definition defs)))]

          ; User-defined function
          [(assoc (car in) defs)
           (make-env ; Replace the `def` name with its body
            (append (cdr (assoc (car in) defs))
                    (cdr in))
            out
            defs)]

          ; Standard library function
          [(assoc (car in) stdlib)
           (let-values ([(in out)
                         ((cadr (assoc (car in) stdlib)) (cdr in) out)])
             (make-env in out defs))]
          
          [else
           (error "Unknown operation --" (car in))])))

(define (eval-env env)
  (if (null? (env-input-stack env))
      env
      (eval-env (step env))))

(define (eval in)
  (env-output-stack
   (eval-env (make-env in '() '()))))

(check-equal? (eval '(4)) '(4))
(check-equal? (eval '(4 5 6)) '(6 5 4))
(check-equal? (eval '(4 5 6 +)) '(11 4))
(check-equal? (eval '(4 5 6 + +)) '(15))

(check-exn exn:fail? (λ () (eval '(4 5 6 + + unknown))))

(check-equal? (eval '(7 dup +)) '(14))

; User defined functions
(check-equal? (eval '(: incr 1 + \; 5 incr)) '(6))
(check-equal? (eval '(5 : incr 1 + \; incr)) '(6))
(check-equal? (eval '(: sq dup * \;
                        5 sq sq)) '(625))

; Function literals
(check-equal? (eval '(1 [1 +] call)) '(2))
