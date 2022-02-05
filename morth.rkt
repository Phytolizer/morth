#lang racket

(define (stack-pop stack)
  (values (car stack) (cdr stack)))

(define (stack-push stack x)
  (list* x stack))

(define (simulate-push x ip stack)
  (values (+ ip 1) (stack-push stack x)))

(define (stack-pop-pair stack)
  (values (car stack) (cadr stack) (cddr stack)))

(define-syntax-rule (basic-op f ip stack)
  (let*-values ([(y x stack) (stack-pop-pair stack)]) (values (+ ip 1) (stack-push stack (f x y)))))

(define (simulate-plus ip stack)
  (basic-op + ip stack))

(define (simulate-minus ip stack)
  (basic-op - ip stack))

(define (simulate-dump ip stack)
  (let-values ([(x stack) (stack-pop stack)])
    (displayln x)
    (values (+ ip 1) stack)))

(define (simulate-program program)
  (define (loop ip stack)
    (if (< ip (length program))
        (let-values ([(ip stack) (let ([instruction (list-ref program ip)])
                                   (case (car instruction)
                                     [(PUSH) (simulate-push (cadr instruction) ip stack)]
                                     [(PLUS) (simulate-plus ip stack)]
                                     [(MINUS) (simulate-minus ip stack)]
                                     [(DUMP) (simulate-dump ip stack)]))])
          (loop ip stack))
        (void)))
  (loop 0 '()))

(simulate-program '((PUSH 34) (PUSH 35) (PLUS) (DUMP) (PUSH 500) (PUSH 80) (MINUS) (DUMP)))
