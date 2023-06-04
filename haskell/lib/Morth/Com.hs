module Morth.Com (compileProgram) where

import Data.Foldable (toList)
import Data.Primitive (Array)
import qualified Data.Text.Lazy as TL
import Morth.Config (memCapacity)
import Morth.Op (Op (..), OpCode (..))

indent :: TL.Text -> TL.Text
indent line = TL.concat ["    ", line]

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

footer :: Int -> [TL.Text]
footer len =
  [ ".L" <> TL.pack (show len) <> ":"
  , "    mov rax, 60"
  , "    mov rdi, 0"
  , "    syscall"
  , "segment .bss"
  , "mem: resb " <> TL.pack (show memCapacity)
  ]

instHeader :: Int -> Op -> [TL.Text]
instHeader ip op =
  [ ".L" <> TL.pack (show ip) <> ":"
  , indent $ ";; -- " <> TL.pack (show $ opCode op) <> " --"
  ]

genInst :: Int -> Op -> [TL.Text]
genInst ip op = case opCode op of
  OpPush x ->
    [ TL.concat ["mov rax, " <> TL.pack (show x)]
    , "push rax"
    ]
  OpDup ->
    [ "pop rax"
    , "push rax"
    , "push rax"
    ]
  Op2Dup ->
    [ "pop rbx"
    , "pop rax"
    , "push rax"
    , "push rbx"
    , "push rax"
    , "push rbx"
    ]
  OpSwap ->
    [ "pop rbx"
    , "pop rax"
    , "push rbx"
    , "push rax"
    ]
  OpDrop ->
    [ "pop rax"
    ]
  OpOver ->
    [ "pop rbx"
    , "pop rax"
    , "push rax"
    , "push rbx"
    , "push rax"
    ]
  OpMem ->
    [ "push mem"
    ]
  OpLoad ->
    [ "pop rax"
    , "xor rbx, rbx"
    , "mov bl, [rax]"
    , "push rbx"
    ]
  OpStore ->
    [ "pop rbx"
    , "pop rax"
    , "mov [rax], bl"
    ]
  OpSyscall0 ->
    [ "pop rax"
    , "syscall"
    ]
  OpSyscall1 ->
    [ "pop rax"
    , "pop rdi"
    , "syscall"
    ]
  OpSyscall2 ->
    [ "pop rax"
    , "pop rdi"
    , "pop rsi"
    , "syscall"
    ]
  OpSyscall3 ->
    [ "pop rax"
    , "pop rdi"
    , "pop rsi"
    , "pop rdx"
    , "syscall"
    ]
  OpSyscall4 ->
    [ "pop rax"
    , "pop rdi"
    , "pop rsi"
    , "pop rdx"
    , "pop r10"
    , "syscall"
    ]
  OpSyscall5 ->
    [ "pop rax"
    , "pop rdi"
    , "pop rsi"
    , "pop rdx"
    , "pop r10"
    , "pop r8"
    , "syscall"
    ]
  OpSyscall6 ->
    [ "pop rax"
    , "pop rdi"
    , "pop rsi"
    , "pop rdx"
    , "pop r10"
    , "pop r8"
    , "pop r9"
    , "syscall"
    ]
  OpPlus ->
    [ "pop rbx"
    , "pop rax"
    , "add rax, rbx"
    , "push rax"
    ]
  OpMinus ->
    [ "pop rbx"
    , "pop rax"
    , "sub rax, rbx"
    , "push rax"
    ]
  OpMod ->
    [ "pop rbx"
    , "pop rax"
    , "xor rdx, rdx"
    , "div rbx"
    , "push rdx"
    ]
  OpEq ->
    [ "pop rbx"
    , "pop rax"
    , "cmp rax, rbx"
    , "sete al"
    , "movzx rax, al"
    , "push rax"
    ]
  OpNe ->
    [ "pop rbx"
    , "pop rax"
    , "cmp rax, rbx"
    , "setne al"
    , "movzx rax, al"
    , "push rax"
    ]
  OpGt ->
    [ "pop rbx"
    , "pop rax"
    , "cmp rax, rbx"
    , "setg al"
    , "movzx rax, al"
    , "push rax"
    ]
  OpLt ->
    [ "pop rbx"
    , "pop rax"
    , "cmp rax, rbx"
    , "setl al"
    , "movzx rax, al"
    , "push rax"
    ]
  OpGe ->
    [ "pop rbx"
    , "pop rax"
    , "cmp rax, rbx"
    , "setge al"
    , "movzx rax, al"
    , "push rax"
    ]
  OpLe ->
    [ "pop rbx"
    , "pop rax"
    , "cmp rax, rbx"
    , "setle al"
    , "movzx rax, al"
    , "push rax"
    ]
  OpShl ->
    [ "pop rcx"
    , "pop rax"
    , "shl rax, cl"
    , "push rax"
    ]
  OpShr ->
    [ "pop rcx"
    , "pop rax"
    , "shr rax, cl"
    , "push rax"
    ]
  OpBand ->
    [ "pop rbx"
    , "pop rax"
    , "and rax, rbx"
    , "push rax"
    ]
  OpBor ->
    [ "pop rbx"
    , "pop rax"
    , "or rax, rbx"
    , "push rax"
    ]
  OpDump ->
    [ "pop rdi"
    , "call dump"
    ]
  OpIf (-1) -> error "invalid jump target"
  OpIf dest ->
    [ "pop rax"
    , "cmp rax, 0"
    , "je .L" <> TL.pack (show dest)
    ]
  OpElse (-1) -> error "invalid jump target"
  OpElse dest ->
    [ "jmp .L" <> TL.pack (show dest)
    ]
  OpWhile -> []
  OpDo (-1) -> error "invalid jump target"
  OpDo dest ->
    [ "pop rax"
    , "cmp rax, 0"
    , "je .L" <> TL.pack (show dest)
    ]
  OpEnd (-1) -> error "invalid jump target"
  OpEnd dest ->
    [ "jmp .L" <> TL.pack (show dest) | dest /= (ip + 1)
    ]

step :: (Int, Op) -> [TL.Text]
step (ip, op) = instHeader ip op <> map indent (genInst ip op)

compileProgram :: Array Op -> TL.Text
compileProgram ops =
  TL.unlines
    ( header
        ++ concatMap step (zip [0 ..] (toList ops))
        ++ footer (length ops)
    )
