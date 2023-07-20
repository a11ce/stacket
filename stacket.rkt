#lang racket

(struct next-as-datum ())

(define (rpn-interp-list prog)
  (define-values
    (result _)
    (for/fold ([stack '()]
               [to-quote #f])
              ([dat prog])
      (cond
        [(next-as-datum? dat) (values stack #t)]
        [(and (not to-quote) (procedure? dat))
         (values (rpn-proc-interp dat stack) #f)]
        [else (values (cons dat stack) #f)])))
  (car result))

(define (rpn-proc-interp dat stack)
  (define arity (procedure-arity dat)) 
  (if (number? arity)
      (rpn-proc-interp-arity dat stack arity)
      (rpn-proc-interp-arity dat (rest stack)
                             (first stack))))

(define (rpn-proc-interp-arity dat stack arity)
  (define-values (args rest-stack) (split-at stack arity))
  (cons (apply dat args) rest-stack))


(define datum (next-as-datum))

(define-syntax-rule
  (eval-rpn (prog ...))
  (rpn-interp-list (list prog ...)))

(define prog (list 3 2 1 3 list datum add1 2 map))

(rpn-interp-list prog)
