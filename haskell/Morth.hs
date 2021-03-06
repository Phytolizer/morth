import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO
  ( IOMode(ReadMode, WriteMode)
  , hClose
  , hGetContents
  , hPutStrLn
  , openFile
  , stderr
  )
import System.Process (callProcess)

data Op
  = OpPush Int
  | OpPlus
  | OpMinus
  | OpDump
  deriving (Show)

pop :: [Int] -> (Int, [Int])
pop [] = error "stack underflow"
pop (x:xs) = (x, xs)

parseTokenAsOp :: String -> Op
parseTokenAsOp token =
  case token of
    "+" -> OpPlus
    "-" -> OpMinus
    "." -> OpDump
    _ -> OpPush (read token :: Int)

loadProgramFromFile :: String -> IO [Op]
loadProgramFromFile inFilePath = do
  inHandle <- openFile inFilePath ReadMode
  contents <- hGetContents inHandle
  return $ map parseTokenAsOp (words contents)

stackOp :: (Int -> Int -> Int) -> [Int] -> [Int]
stackOp op stack =
  let (y, stack') = pop stack
      (x, stack2) = pop stack'
   in (op x y : stack2)

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
              let stack' = stackOp (+) stack
                  ip' = ip + 1
               in loop ip' stack'
            OpMinus -> do
              let stack' = stackOp (-) stack
                  ip' = ip + 1
               in loop ip' stack'
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
                   ["pop rbx", "pop rax", "sub rax, rbx", "push rax"]
               OpDump -> mapM_ (hPutStrLn out) ["pop rdi", "call dump"])
            loop remProgram'
   in do loop program
         mapM_ (hPutStrLn out) ["mov rax, 60", "mov rdi, 0", "syscall"]
         hClose out
         callProcess "nasm" ["-felf64", "output.asm"]
         callProcess "ld" ["-o", "output", "output.o"]
         return ()

usage :: IO ()
usage = do
  name <- getProgName
  hPutStrLn stderr $ "usage: " ++ name ++ " <SUBCOMMAND> [ARGS]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    (subcommand:args') ->
      case subcommand of
        "sim" ->
          case args' of
            (inFilePath:_) -> do
              program <- loadProgramFromFile inFilePath
              simulateProgram program
            [] -> do
              usage
              hPutStrLn stderr "ERROR: no input file provided for simulation"
              exitFailure
        "com" ->
          case args' of
            (inFilePath:_) -> do
              program <- loadProgramFromFile inFilePath
              compileProgram program "output.asm"
        _ -> do
          usage
          hPutStrLn stderr ("ERROR: unknown subcommand '" ++ subcommand ++ "'")
          exitFailure
    [] -> do
      usage
      hPutStrLn stderr "ERROR: no subcommand provided"
      exitFailure
