import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (IOMode(WriteMode), hClose, hPutStrLn, openFile, stderr)

data Op
  = OpPush Int
  | OpPlus
  | OpMinus
  | OpDump
  deriving (Show)

pop :: [Int] -> (Int, [Int])
pop [] = error "stack underflow"
pop (x:rest) = (x, rest)

simulateProgram :: [Op] -> IO ()
simulateProgram program = do
  let loop ip stack = do
        when (ip < length program) $ do
          case program !! ip of
            OpPush x -> do
              let stack' = (x : stack)
                  ip' = ip + 1
               in loop ip' stack'
            OpPlus -> do
              let (y, stack') = pop stack
                  (x, stack'') = pop stack'
                  stack3 = (x + y : stack'')
                  ip' = ip + 1
               in loop ip' stack3
            OpMinus -> do
              let (y, stack') = pop stack
                  (x, stack'') = pop stack'
                  stack3 = (x - y : stack'')
                  ip' = ip + 1
               in loop ip' stack3
            OpDump -> do
              let (x, stack') = pop stack
                  ip' = ip + 1
               in do print x
                     loop ip' stack'
   in loop 0 []

compileProgram :: [Op] -> String -> IO ()
compileProgram program outFilePath = do
  out <- openFile outFilePath WriteMode
  mapM_
    (hPutStrLn out)
    [ "segment .text"
    , "dump:"
    , "sub     rsp, 40"
    , "lea     rsi, [rsp + 31]"
    , "mov     byte [rsp + 31], 10"
    , "mov     ecx, 1"
    , "mov     r8, -3689348814741910323"
    , ".LBB0_1:"
    , "mov     rax, rdi"
    , "mul     r8"
    , "shr     rdx, 3"
    , "lea     eax, [rdx + rdx]"
    , "lea     r9d, [rax + 4*rax]"
    , "mov     eax, edi"
    , "sub     eax, r9d"
    , "or      al, 48"
    , "mov     byte [rsi - 1], al"
    , "add     rsi, -1"
    , "add     rcx, 1"
    , "cmp     rdi, 9"
    , "mov     rdi, rdx"
    , "ja      .LBB0_1"
    , "mov     edi, 1"
    , "mov     rdx, rcx"
    , "mov     rax, 1"
    , "syscall"
    , "add     rsp, 40"
    , "ret"
    , "global _start"
    , "_start:"
    ]
  let loop remProgram =
        case remProgram of
          [] -> return ()
          (op:remProgram') -> do
            hPutStrLn out (";; -- " ++ show op ++ " --")
            (case op of
               OpPush x -> hPutStrLn out ("push " ++ show x)
               OpPlus ->
                 mapM_
                   (hPutStrLn out)
                   ["pop rbx", "pop rax", "add rax, rbx", "push rax"]
               OpMinus ->
                 mapM_
                   (hPutStrLn out)
                   ["pop rbx", "pop rax", "add rax, rbx", "push rax"]
               OpDump -> mapM_ (hPutStrLn out) ["pop rdi", "call dump"])
            loop remProgram'
   in do loop program
         mapM_ (hPutStrLn out) ["mov rax, 60", "mov rdi, 0", "syscall"]
         hClose out

program :: [Op]
program =
  [OpPush 34, OpPush 35, OpPlus, OpDump, OpPush 500, OpPush 80, OpMinus, OpDump]

usage :: IO ()
usage = do
  name <- getProgName
  hPutStrLn stderr $ "usage: " ++ name ++ " <SUBCOMMAND> [ARGS]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [subcommand] ->
      case subcommand of
        "sim" -> simulateProgram program
        "com" -> compileProgram program "output.asm"
        _ -> do
          usage
          exitFailure
    _ -> do
      usage
      exitFailure
