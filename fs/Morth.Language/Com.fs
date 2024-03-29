module Morth.Language.Com

let indent s = "    " + s

let handleOp ip =
  function
  | Op.Push n -> [ sprintf "push %d" n ]
  | Op.Dup ->
    [
      "pop rax"
      "push rax"
      "push rax"
    ]
  | Op.Plus ->
    [
      "pop rbx"
      "pop rax"
      "add rax, rbx"
      "push rax"
    ]
  | Op.Minus ->
    [
      "pop rbx"
      "pop rax"
      "sub rax, rbx"
      "push rax"
    ]
  | Op.Eq ->
    [
      "pop rbx"
      "pop rax"
      "cmp rax, rbx"
      "sete al"
      "movzx rax, al"
      "push rax"
    ]
  | Op.Gt ->
    [
      "pop rbx"
      "pop rax"
      "cmp rax, rbx"
      "setg al"
      "movzx rax, al"
      "push rax"
    ]
  | Op.Dump ->
    [
      "pop rdi"
      "call dump"
    ]
  | Op.If dest ->
    [
      "pop rax"
      "test rax, rax"
      sprintf "jz .L%d" dest
    ]
  | Op.Else dest -> [ sprintf "jmp .L%d" dest ]
  | Op.While -> []
  | Op.Do dest ->
    [
      "pop rax"
      "test rax, rax"
      sprintf "jz .L%d" dest
    ]
  | Op.End dest -> if dest <> ip + 1 then [ sprintf "jmp .L%d" dest ] else []
  | Op.Mem -> [ "push mem" ]
  | Op.Load ->
    [
      "pop rax"
      "mov bl, [rax]"
      "movzx rax, bl"
      "push rax"
    ]
  | Op.Store ->
    [
      "pop rbx"
      "pop rax"
      "mov [rax], bl"
    ]
  | Op.Syscall1 ->
    [
      "pop rax"
      "pop rdi"
      "syscall"
    ]
  | Op.Syscall2 ->
    [
      "pop rax"
      "pop rdi"
      "pop rsi"
      "syscall"
    ]
  | Op.Syscall3 ->
    [
      "pop rax"
      "pop rdi"
      "pop rsi"
      "pop rdx"
      "syscall"
    ]
  | Op.Syscall4 ->
    [
      "pop rax"
      "pop rdi"
      "pop rsi"
      "pop rdx"
      "pop r10"
      "syscall"
    ]
  | Op.Syscall5 ->
    [
      "pop rax"
      "pop rdi"
      "pop rsi"
      "pop rdx"
      "pop r10"
      "pop r8"
      "syscall"
    ]
  | Op.Syscall6 ->
    [
      "pop rax"
      "pop rdi"
      "pop rsi"
      "pop rdx"
      "pop r10"
      "pop r8"
      "pop r9"
      "syscall"
    ]

let compileOp ip (op : Op.t) =
  List.append
    [
      sprintf ".L%d:" ip
      sprintf ";; -- %A --" op.code
    ]
    (handleOp ip op.code)
  |> List.toSeq
  |> Seq.map indent

let header =
  List.toSeq
    [
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
    ]

let footer =
  List.toSeq
    [
      "    mov rax, 60"
      "    mov rdi, 0"
      "    syscall"
      "segment .bss"
      sprintf "mem: resb %d" Config.MEM_CAPACITY
    ]

let compile program =
  [
    header
    Seq.mapi compileOp program |> Seq.concat
    Seq.singleton (sprintf ".L%d:" (Seq.length program))
    footer
  ]
  |> List.toSeq
  |> Seq.concat
  |> Seq.map (fun s -> s + "\n")
  |> String.concat ""
