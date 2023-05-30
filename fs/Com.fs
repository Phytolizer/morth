module Morth.Com

let indent s = "    " + s

let compileOp op =
  Array.append
    [| (sprintf ";; -- %A --" op) |]
    (match op with
     | Op.Push n -> [| sprintf "push %d" n |]
     | Op.Plus ->
       [|
         "pop rbx"
         "pop rax"
         "add rax, rbx"
         "push rax"
       |]
     | Op.Minus ->
       [|
         "pop rbx"
         "pop rax"
         "sub rax, rbx"
         "push rax"
       |]
     | Op.Dump ->
       [|
         "pop rdi"
         "call dump"
       |])
  |> Array.map indent
  |> Array.toSeq

let header =
  Array.toSeq
    [|
      "segment .text"
      "dump:"
      "    mov     r9, -3689348814741910323"
      "    sub     rsp, 40"
      "    mov     BYTE [rsp+31], 10"
      "    lea     rcx, [rsp+30]"
      ".L2:"
      "    mov     rax, rdi"
      "    lea     r8, [rsp+32]"
      "    mul     r9"
      "    mov     rax, rdi"
      "    sub     r8, rcx"
      "    shr     rdx, 3"
      "    lea     rsi, [rdx+rdx*4]"
      "    add     rsi, rsi"
      "    sub     rax, rsi"
      "    add     eax, 48"
      "    mov     BYTE [rcx], al"
      "    mov     rax, rdi"
      "    mov     rdi, rdx"
      "    mov     rdx, rcx"
      "    sub     rcx, 1"
      "    cmp     rax, 9"
      "    ja      .L2"
      "    lea     rax, [rsp+32]"
      "    mov     edi, 1"
      "    sub     rdx, rax"
      "    xor     eax, eax"
      "    lea     rsi, [rsp+32+rdx]"
      "    mov     rdx, r8"
      "    mov     rax, 1"
      "    syscall"
      "    add     rsp, 40"
      "    ret"
      "global _start"
      "_start:"
    |]

let footer =
  Array.toSeq
    [|
      "    mov rax, 60"
      "    mov rdi, 0"
      "    syscall"
    |]

let compile program =
  [|
    header
    Seq.map compileOp program |> Seq.concat
    footer
  |]
  |> Array.toSeq
  |> Seq.concat
  |> Seq.map (fun s -> s + "\n")
  |> String.concat ""