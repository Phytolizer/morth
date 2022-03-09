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

(define-syntax-rule (emit out) (lambda (text) (displayln text out)))

(define (compile-push x out)
  (map (emit out) (list (string-append-immutable "push " (number->string x)))))

(define (compile-plus out)
  (map (emit out) '("pop rbx" "pop rax" "add rax, rbx" "push rax")))

(define (compile-minus out)
  (map (emit out) '("pop rbx" "pop rax" "sub rax, rbx" "push rax")))

(define (compile-dump out)
  (map (emit out) '("pop rdi" "call dump")))

(define (compile-program program out)
  (define (loop remaining-program)
    (if (null? remaining-program)
        (void)
        (let ([instruction (car remaining-program)])
          (begin
            ((emit out) (string-append-immutable ";; -- " (symbol->string (car instruction)) " --"))
            (case (car instruction)
              [(PUSH) (compile-push (cadr (car remaining-program)) out)]
              [(PLUS) (compile-plus out)]
              [(MINUS) (compile-minus out)]
              [(DUMP) (compile-dump out)])
            (loop (cdr remaining-program))))))
  (map (emit out)
       '("segment .text" "dump:"
                         "sub     rsp, 40"
                         "lea     rsi, [rsp + 31]"
                         "mov     byte [rsp + 31], 10"
                         "mov     ecx, 1"
                         "mov     r8, -3689348814741910323"
                         ".LBB0_1:"
                         "mov     rax, rdi"
                         "mul     r8"
                         "shr     rdx, 3"
                         "lea     eax, [rdx + rdx]"
                         "lea     r9d, [rax + 4*rax]"
                         "mov     eax, edi"
                         "sub     eax, r9d"
                         "or      al, 48"
                         "mov     byte [rsi - 1], al"
                         "add     rsi, -1"
                         "add     rcx, 1"
                         "cmp     rdi, 9"
                         "mov     rdi, rdx"
                         "ja      .LBB0_1"
                         "mov     edi, 1"
                         "mov     rdx, rcx"
                         "mov     rax, 1"
                         "syscall"
                         "add     rsp, 40"
                         "ret"
                         "global _start"
                         "_start:"))
  (loop program)
  (map (emit out) '("mov rax, 60" "mov rdi, 0" "syscall"))
  (void))

(define program '((PUSH 34) (PUSH 35) (PLUS) (DUMP) (PUSH 500) (PUSH 80) (MINUS) (DUMP)))

(define (run-command args)
  (let ([command-path (find-executable-path (car args))])
    (if (false? command-path)
        (begin
          (displayln (string-append-immutable "ERROR: '" (car args) "' could not be found")
                     (current-error-port))
          (exit 1))
        (begin
          (displayln (string-join args " "))
          (let-values ([(process stdout _in _err)
                        (apply subprocess #f #f 'stdout command-path (cdr args))])
            (display (port->string stdout))
            (close-input-port stdout)
            (subprocess-wait process)
            (if (not (eq? (subprocess-status process) 0))
                (begin
                  (displayln "ERROR: process reported failure")
                  (exit 1))
                (void)))))))

(define (usage file)
  (map (lambda (s) (displayln s file))
       '("Usage: morth <SUBCOMMAND> [ARGS]" "  SUBCOMMANDS:"
                                            "    sim      Simulate the program."
                                            "    com      Compile the program.")))

(let ([args (vector->list (current-command-line-arguments))])
  (if (null? args)
      (begin
        (usage (current-error-port))
        (displayln "ERROR: no subcommand is provided" (current-error-port))
        (exit 1))
      (let-values ([(subcommand args) (stack-pop args)])
        (case subcommand
          [("sim") (simulate-program program)]
          [("com")
           (begin
             (call-with-output-file "output.asm"
               (lambda (out) (compile-program program out))
               #:mode 'text
               #:exists 'replace)
             (run-command '("nasm" "-felf64" "output.asm"))
             (run-command '("ld" "-o" "output" "output.o")))]))))
