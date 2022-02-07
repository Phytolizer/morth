open System
open System.IO
open System.Text
open System.Collections.Generic

type Op =
    | OpPush of UInt64
    | OpPlus
    | OpMinus
    | OpDump

let simulateProgram(program : Op[]) =
    let stack = new Stack<UInt64>()
    let rec stepProgram ip =
        if ip < program.Length then
            let ip = match program.[ip] with
                     | OpPush x ->
                         stack.Push x;
                         ip + 1
                     | OpPlus ->
                         let b = stack.Pop();
                         let a = stack.Pop();
                         stack.Push (a + b);
                         ip + 1
                     | OpMinus ->
                         let b = stack.Pop();
                         let a = stack.Pop();
                         stack.Push (a - b);
                         ip + 1
                     | OpDump ->
                         let x = stack.Pop();
                         printfn "%u" x;
                         ip + 1
            stepProgram ip
        else ()
    stepProgram 0

let compileProgram (program : Op[]) outFilePath =
    using (File.Create outFilePath) (fun out ->
        let addText (text : string) =
            let text = $"{text}\n"
            let info = UTF8Encoding(true).GetBytes(text)
            out.Write(info, 0, info.Length)
        Array.map addText [|
            "segment .text"
            "dump:"
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
            "_start:"
        |] |> ignore
        let rec stepProgram ip =
            if ip < program.Length then
                addText $";; -- {program.[ip]} --"
                match program.[ip] with
                    | OpPush x ->
                        addText $"push {x}"
                    | OpPlus ->
                        addText "pop rbx"
                        addText "pop rax"
                        addText "add rax, rbx"
                        addText "push rax"
                    | OpMinus ->
                        addText "pop rbx"
                        addText "pop rax"
                        addText "sub rax, rbx"
                        addText "push rax"
                    | OpDump ->
                        addText "pop rdi"
                        addText "call dump"
                stepProgram (ip + 1)
            else
                ()
        stepProgram 0;
        addText "mov rax, 60"
        addText "mov rdi, 0"
        addText "syscall"
    )


[<EntryPoint>]
let main(args : string[]) =
    let program = [|
        OpPush 34uL
        OpPush 35uL
        OpPlus
        OpDump
    |]
    simulateProgram program
    compileProgram program "output.asm"
    0
