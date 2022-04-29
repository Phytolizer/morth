; Source: https://gist.github.com/steakhal/c30ef3658172b6f2ed74157ecbb000c4
; The stack pointer is going to be an index into the stack, and the
; stack is an array of words. The alternative would be to have the
; stack pointer me a pointer to memory, but this is perhaps a bit
; nicer, as where the stack actually lives is totally irrelevant.

@stack = global [1000 x i64] undef
@sp    = global i64 undef;

; Now we have the basic stack operations: push, pop, and peek. As can
; be seen from the definitions, LLVM is typed, which is really nice as
; it allows a large class of errors to be detected at compile-time.

define void @push(i64 %val) {
    ; We need to figure out the actual address of the stack pointer
    ; location, as it's just an index into the stack. Fortunately,
    ; LLVM has a function `getelementptr` which allows us to navigate
    ; complex structures.

    ; Note that @sp is not used directly. This is because global
    ; variables are actually pointers to memory where the value is
    ; stored. Thus, all access to globals must be via loads and
    ; stores.

    %sp   = load i64, i64* @sp
    %addr = getelementptr [1000 x i64], [1000 x i64]* @stack, i64 0, i64 %sp

    ; Now we store the value. Note that type information must be
    ; explicit, even if it could be inferred from context.

    store i64 %val, i64* %addr

    ; Finally, we update the stack pointer. Literals do not get type
    ; annotations. Assignment is just not a thing: it only happens
    ; when declaring a variable, so we must calculate and then store
    ; the new value.

    %newsp = add i64 %sp, 1
    store i64 %newsp, i64* @sp

    ; Finally, we return. All functions, even void, must have a return
    ; statement.

    ret void
}

define i64 @peek() {
    ; As @sp is pointing to the next blank space after the head of the
    ; stack, we need to decrement it to get the head element.

    %sp    = load i64, i64* @sp
    %topsp = sub i64 %sp, 1
    %addr  = getelementptr [1000 x i64], [1000 x i64]* @stack, i64 0, i64 %topsp
    %val   = load i64, i64* %addr

    ret i64 %val
}

define i64 @pop() {
    ; Function calling, of course, makes use of seemingly needless
    ; type annotations. I guess this does simplify compilation, as all
    ; of the types are just *there*, and can be trivially
    ; checked. Perhaps surprisingly, the type is a pointer type: this
    ; is because `call` takes a pointer to the function (because @peek
    ; is a global, and thus a pointer).

    %val = call i64 @peek()

    %sp    = load i64, i64* @sp
    %newsp = sub i64 %sp, 1
    store i64 %newsp, i64* @sp

    ret i64 %val
}

; Now to test it by pushing, popping, and printing some values.

; The `declare` keyword allows the declaration of external globals
; which can then be used, and will be resolved by the linker.

;declare i64 @printf(i8* noalias nocapture, ...)

; String literals are defined as global constants.
;  - private = only directly accessible to things in this module
;  - unnamed_addr = the address doesn't matter, only the value

;@welcome = private unnamed_addr constant [49 x i8] c"barrucadu's wonderful LLVM stack implementation\0A\00"
;@pushing = private unnamed_addr constant [12 x i8] c"Pushing %i\0A\00"
;@popped  = private unnamed_addr constant [11 x i8] c"Popped %i\0A\00"

;define i64 @main() {
;    ; printf doesn't take an array of characters, it takes a pointer
;    ; to the start of an array of characters! So, we have to get one
;    ; using our old friend `getelementptr`.
;
;    %welcomeptr = getelementptr [49 x i8], [49 x i8]* @welcome, i64 0, i64 0
;    call i64 (i8*, ...) @printf(i8* %welcomeptr)
;
;    ; Let's try pushing some values
;    %pushingptr = getelementptr [12 x i8], [12 x i8]* @pushing, i64 0, i64 0
;
;    call i64(i8*, ...) @printf(i8* %pushingptr, i64 1)
;    call void(i64) @push(i64 1)
;
;    call i64(i8*, ...) @printf(i8* %pushingptr, i64 10)
;    call void(i64) @push(i64 10)
;
;    call i64(i8*, ...) @printf(i8* %pushingptr, i64 100)
;    call void(i64) @push(i64 100)
;
;    ; And popping them again
;    %poppedptr = getelementptr [11 x i8], [11 x i8]* @popped, i64 0, i64 0
;
;    %first  = call i64() @pop()
;    %second = call i64() @pop()
;    %third  = call i64() @pop()
;
;    call i64(i8*, ...) @printf(i8* %poppedptr, i64 %first)
;    call i64(i8*, ...) @printf(i8* %poppedptr, i64 %second)
;    call i64(i8*, ...) @printf(i8* %poppedptr, i64 %third)
;
;    ; Return EXIT_SUCCESS
;    ret i64 0
;}
