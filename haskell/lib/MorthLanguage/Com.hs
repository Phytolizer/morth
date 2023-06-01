module MorthLanguage.Com (compileProgram) where

import MorthLanguage.Op (Op (..))

header :: [String]
header =
  [ "segment .text"
  , "dump:"
  , "    mov     r9, -3689348814741910323"
  , "    sub     rsp, 40"
  , "    mov     BYTE [rsp+31], 10"
  , "    lea     rcx, [rsp+30]"
  , ".L2:"
  , "    mov     rax, rdi"
  , "    lea     r8, [rsp+32]"
  , "    mul     r9"
  , "    mov     rax, rdi"
  , "    sub     r8, rcx"
  , "    shr     rdx, 3"
  , "    lea     rsi, [rdx+rdx*4]"
  , "    add     rsi, rsi"
  , "    sub     rax, rsi"
  , "    add     eax, 48"
  , "    mov     BYTE [rcx], al"
  , "    mov     rax, rdi"
  , "    mov     rdi, rdx"
  , "    mov     rdx, rcx"
  , "    sub     rcx, 1"
  , "    cmp     rax, 9"
  , "    ja      .L2"
  , "    lea     rax, [rsp+32]"
  , "    mov     edi, 1"
  , "    sub     rdx, rax"
  , "    xor     eax, eax"
  , "    lea     rsi, [rsp+32+rdx]"
  , "    mov     rdx, r8"
  , "    mov     rax, 1"
  , "    syscall"
  , "    add     rsp, 40"
  , "    ret"
  , "global _start"
  , "_start:"
  ]

footer :: [String]
footer =
  [ "    mov rax, 60"
  , "    mov rdi, 0"
  , "    syscall"
  ]

step :: Op -> [String]
step (OpPush x) = ["    push " ++ show x]
step OpPlus =
  [ "    pop rbx"
  , "    pop rax"
  , "    add rax, rbx"
  , "    push rax"
  ]
step OpMinus =
  [ "    pop rbx"
  , "    pop rax"
  , "    sub rax, rbx"
  , "    push rax"
  ]
step OpDump =
  [ "    pop rdi"
  , "    call dump"
  ]

compileProgram :: [Op] -> String
compileProgram ops = unlines (header ++ concatMap step ops ++ footer)
