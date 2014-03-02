module Main where

import           Interpreter.Data
import           Interpreter.Eval
import           Interpreter.Kernel (kernelFunctions)
import           Interpreter.Parser
import           System.IO          (hFlush, stdout)

main :: IO ()
main = startSession

startSession :: IO ()
startSession = do
        case initializeNS of
                (Right o) -> do _ <- repl o
                                return ()
                _         -> putStrLn "Error loading kernel functions."

initializeNS :: Either ErrorMessage Obj
initializeNS = insertSymbol "Kernel" emptyNS emptyNS >>= insertSymbol "Kernel.Args" emptyNS >>= importSymbols kernelFunctions

repl :: Obj -> IO ImpureEvaluation
repl root = do
        putStr ">> "
        hFlush stdout
        i <- getLine
        let EitherT impureE = eval i root in
                do e <- impureE
                   case e of
                        Left m -> do
                                putStrLn m
                                repl root
                        Right r -> do
                                putStrLn (getMessage r)
                                repl r

parseTest :: String -> IO ()
parseTest i = 
        case parseExpressions i of
                Right x -> putStrLn $ show x
                Left m -> putStrLn (show m)
        

parseTestFile :: String -> IO ()
parseTestFile name = do
        i <- readFile name
        case parsePandoc i of
                Right x -> putStrLn $ show x
                Left m -> putStrLn (show m)


