module Main where

import Interpreter.Data
import Interpreter.Parser
import Interpreter.Eval
import System.IO (stdout, hFlush)
import Interpreter.Kernel (kernelFunctions)

main :: IO ()
main = startSession

startSession :: IO ()
startSession = do
        case (insertSymbol "Kernel" emptyNS emptyNS >>= insertSymbol "Kernel.Args" emptyNS >>= importSymbols kernelFunctions) of
                (Right o) -> do _ <- repl o
                                return ()
                _         -> putStrLn "Error loading kernel functions."

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

parseTest :: IO ()
parseTest = do
        putStr ">> "
        hFlush stdout
        i <- getLine
        case parseExpression i of
                Right x -> putStrLn $ show x
                Left m -> putStrLn (show m)
        parseTest
        