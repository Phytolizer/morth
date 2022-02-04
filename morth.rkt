#lang racket

(define (push x)
  (list 'PUSH x))

(define (plus)
  '(PLUS))

(define (dump)
  '(DUMP))

(define (simulate-program program)
  (let ([loop (lambda (program ip stack)
                (let ([instruction (list-ref program ip)])
                  (case (car instruction)
                    ['PUSH (simulate-push )])))])
    (loop program 0 #())))

(simulate-program (list (push 34) (push 35) (plus)))
