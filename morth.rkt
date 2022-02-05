#lang racket

(define (push x)
  (list 'PUSH x))

(define (plus)
  '(PLUS))

(define (dump)
  '(DUMP))

(define (stack-pop stack)
  (values (car stack) (cdr stack)))

(define (stack-push stack x)
  (list* x stack))

(define (simulate-push x ip stack)
  (values (+ ip 1) (stack-push stack x)))

(define (simulate-plus ip stack)
  (let*-values ([(y stack) (stack-pop stack)] [(x stack) (stack-pop stack)])
    (values (+ ip 1) (stack-push stack (+ x y)))))

(define (simulate-dump ip stack)
  (let-values ([(x stack) (stack-pop stack)])
    (displayln x)
    (values (+ ip 1) stack)))

(define (simulate-program program)
  (define (loop program ip stack)
    (if (< ip (length program))
        (let-values ([(ip stack) (let ([instruction (list-ref program ip)])
                                   (case (car instruction)
                                     ['PUSH (simulate-push (cadr instruction) ip stack)]
                                     ['PLUS (simulate-plus ip stack)]
                                     ['DUMP (simulate-dump ip stack)]))])
          (loop program ip stack))
        (void)))
  (loop program 0 '()))

(simulate-program (list (push 34) (push 35) (plus) (dump)))
