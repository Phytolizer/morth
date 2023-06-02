module MorthLanguage.Com (compileProgram) where

import qualified Data.Text.Lazy as TL
import MorthLanguage.Op (Op (..))

header :: [TL.Text]
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

footer :: [TL.Text]
footer =
  [ "    mov rax, 60"
  , "    mov rdi, 0"
  , "    syscall"
  ]

step :: Op -> [TL.Text]
step op =
  ["    ;; -- " <> TL.pack (show op) <> " --"]
    <> case op of
      OpPush x -> [TL.concat ["    push " <> TL.pack (show x)]]
      OpPlus ->
        [ "    pop rbx"
        , "    pop rax"
        , "    add rax, rbx"
        , "    push rax"
        ]
      OpMinus ->
        [ "    pop rbx"
        , "    pop rax"
        , "    sub rax, rbx"
        , "    push rax"
        ]
      OpDump ->
        [ "    pop rdi"
        , "    call dump"
        ]

compileProgram :: [Op] -> TL.Text
compileProgram ops = TL.unlines (header ++ concatMap step ops ++ footer)
