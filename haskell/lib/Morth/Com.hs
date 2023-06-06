module Morth.Com (compileProgram) where

import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Primitive (Array)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Word (Word8)
import Morth.Config (memCapacity)
import Morth.Op (Op (..), OpCode (..))
import Text.Printf (printf)

indent :: TL.Text -> TL.Text
indent line = TL.concat ["    ", line]

header :: [TL.Text]
header =
  [ "segment .text"
  , "print:"
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

footer :: Int -> [BL.ByteString] -> [TL.Text]
footer len strs =
  [ ".L" <> TL.pack (show len) <> ":"
  , "    mov rax, 60"
  , "    mov rdi, 0"
  , "    syscall"
  , "segment .data"
  ]
    ++ emitStrs (zip [0 ..] strs)
    ++ [ "segment .bss"
       , "mem: resb " <> TL.pack (show memCapacity)
       ]
 where
  emitStrs :: [(Int, BL.ByteString)] -> [TL.Text]
  emitStrs [] = []
  emitStrs ((i, bs) : rest) =
    ["porth_str_" <> TL.pack (show i) <> ": db " <> hexify bs]
      <> emitStrs rest

  hexify :: BL.ByteString -> TL.Text
  hexify bs = TL.intercalate "," $ map hexifyByte $ BL.unpack bs

  hexifyByte :: Word8 -> TL.Text
  hexifyByte b = TL.pack $ printf "0x%02x" b

instHeader :: Int -> Op -> [TL.Text]
instHeader ip op =
  [ ".L" <> TL.pack (show ip) <> ":"
  , indent $ ";; -- " <> TL.pack (show $ opCode op) <> " --"
  ]

genInst :: Int -> Op -> [BL.ByteString] -> ([BL.ByteString], [TL.Text])
genInst ip op strs = case opCode op of
  OpPushInt x ->
    ( strs
    ,
      [ TL.concat ["mov rax, " <> TL.pack (show x)]
      , "push rax"
      ]
    )
  OpPushStr s ->
    let bs = encodeUtf8 s
     in ( strs ++ [bs]
        ,
          [ "mov rax, " <> TL.pack (show $ BL.length bs)
          , "push rax"
          , "push porth_str_" <> TL.pack (show $ length strs)
          ]
        )
  OpDup ->
    ( strs
    ,
      [ "pop rax"
      , "push rax"
      , "push rax"
      ]
    )
  Op2Dup ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "push rax"
      , "push rbx"
      , "push rax"
      , "push rbx"
      ]
    )
  OpSwap ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "push rbx"
      , "push rax"
      ]
    )
  OpDrop ->
    ( strs
    ,
      [ "pop rax"
      ]
    )
  OpOver ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "push rax"
      , "push rbx"
      , "push rax"
      ]
    )
  OpMem ->
    ( strs
    ,
      [ "push mem"
      ]
    )
  OpLoad ->
    ( strs
    ,
      [ "pop rax"
      , "xor rbx, rbx"
      , "mov bl, [rax]"
      , "push rbx"
      ]
    )
  OpStore ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "mov [rax], bl"
      ]
    )
  OpSyscall0 ->
    ( strs
    ,
      [ "pop rax"
      , "syscall"
      , "push rax"
      ]
    )
  OpSyscall1 ->
    ( strs
    ,
      [ "pop rax"
      , "pop rdi"
      , "syscall"
      , "push rax"
      ]
    )
  OpSyscall2 ->
    ( strs
    ,
      [ "pop rax"
      , "pop rdi"
      , "pop rsi"
      , "syscall"
      , "push rax"
      ]
    )
  OpSyscall3 ->
    ( strs
    ,
      [ "pop rax"
      , "pop rdi"
      , "pop rsi"
      , "pop rdx"
      , "syscall"
      , "push rax"
      ]
    )
  OpSyscall4 ->
    ( strs
    ,
      [ "pop rax"
      , "pop rdi"
      , "pop rsi"
      , "pop rdx"
      , "pop r10"
      , "syscall"
      , "push rax"
      ]
    )
  OpSyscall5 ->
    ( strs
    ,
      [ "pop rax"
      , "pop rdi"
      , "pop rsi"
      , "pop rdx"
      , "pop r10"
      , "pop r8"
      , "syscall"
      , "push rax"
      ]
    )
  OpSyscall6 ->
    ( strs
    ,
      [ "pop rax"
      , "pop rdi"
      , "pop rsi"
      , "pop rdx"
      , "pop r10"
      , "pop r8"
      , "pop r9"
      , "syscall"
      , "push rax"
      ]
    )
  OpPlus ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "add rax, rbx"
      , "push rax"
      ]
    )
  OpMinus ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "sub rax, rbx"
      , "push rax"
      ]
    )
  OpMod ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "xor rdx, rdx"
      , "div rbx"
      , "push rdx"
      ]
    )
  OpEq ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "cmp rax, rbx"
      , "sete al"
      , "movzx rax, al"
      , "push rax"
      ]
    )
  OpNe ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "cmp rax, rbx"
      , "setne al"
      , "movzx rax, al"
      , "push rax"
      ]
    )
  OpGt ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "cmp rax, rbx"
      , "setg al"
      , "movzx rax, al"
      , "push rax"
      ]
    )
  OpLt ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "cmp rax, rbx"
      , "setl al"
      , "movzx rax, al"
      , "push rax"
      ]
    )
  OpGe ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "cmp rax, rbx"
      , "setge al"
      , "movzx rax, al"
      , "push rax"
      ]
    )
  OpLe ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "cmp rax, rbx"
      , "setle al"
      , "movzx rax, al"
      , "push rax"
      ]
    )
  OpShl ->
    ( strs
    ,
      [ "pop rcx"
      , "pop rax"
      , "shl rax, cl"
      , "push rax"
      ]
    )
  OpShr ->
    ( strs
    ,
      [ "pop rcx"
      , "pop rax"
      , "shr rax, cl"
      , "push rax"
      ]
    )
  OpBand ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "and rax, rbx"
      , "push rax"
      ]
    )
  OpBor ->
    ( strs
    ,
      [ "pop rbx"
      , "pop rax"
      , "or rax, rbx"
      , "push rax"
      ]
    )
  OpPrint ->
    ( strs
    ,
      [ "pop rdi"
      , "call print"
      ]
    )
  OpIf (-1) -> error "invalid jump target"
  OpIf dest ->
    ( strs
    ,
      [ "pop rax"
      , "cmp rax, 0"
      , "je .L" <> TL.pack (show dest)
      ]
    )
  OpElse (-1) -> error "invalid jump target"
  OpElse dest ->
    ( strs
    ,
      [ "jmp .L" <> TL.pack (show dest)
      ]
    )
  OpWhile -> (strs, [])
  OpDo (-1) -> error "invalid jump target"
  OpDo dest ->
    ( strs
    ,
      [ "pop rax"
      , "cmp rax, 0"
      , "je .L" <> TL.pack (show dest)
      ]
    )
  OpEnd (-1) -> error "invalid jump target"
  OpEnd dest ->
    ( strs
    , [ "jmp .L" <> TL.pack (show dest) | dest /= (ip + 1)
      ]
    )

step :: Int -> Op -> [BL.ByteString] -> ([BL.ByteString], [TL.Text])
step ip op strs =
  let (strs', inst) = genInst ip op strs
   in (strs', instHeader ip op <> map indent inst)

stepAll :: [Op] -> ([BL.ByteString], [TL.Text])
stepAll ops =
  stepOp [] (zip [0 ..] ops)
 where
  stepOp :: [BL.ByteString] -> [(Int, Op)] -> ([BL.ByteString], [TL.Text])
  stepOp strs [] = (strs, [])
  stepOp strs ((ip, op) : rest) =
    let (strs', inst) = step ip op strs
        (finalStrs, finalInsts) = stepOp strs' rest
     in (finalStrs, inst ++ finalInsts)

compileProgram :: Array Op -> TL.Text
compileProgram ops =
  let (strs, insts) = stepAll (toList ops)
   in TL.unlines
        ( header
            ++ insts
            ++ footer (length ops) strs
        )
