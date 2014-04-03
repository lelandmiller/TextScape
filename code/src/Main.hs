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
        initNS <- runEitherT initializeNS
        case initNS of
                (Right o) -> do _ <- repl o; return ()
                _         -> putStrLn "Error loading kernel functions."

startShell :: IO ()
startShell = do
        initNS <- runEitherT initializeNS
        case initNS of
                (Right o) -> do _ <- shell o; return ()
                _         -> putStrLn "Error loading kernel functions."

initializeNS :: ImpureEvaluation
initializeNS = evalToImpure (importSymbols kernelFunctions emptyNS)
                >>= insertSymbolImpure "Kernel" emptyNS 
                >>= insertSymbolImpure "Kernel.Args" emptyNS 
                >>= eval "(eval /pandoc/ (openFile /stdlib.ts/))"

repl :: Obj -> IO ImpureEvaluation
repl root = do
        hFlush stdout
        i <- getLine
        let EitherT impureE = eval i root in
                do e <- impureE
                   case e of
                        Left m -> do
                                putStrLn m
                                putStrLn "\0"
                                repl root
                        Right r -> do
                                putStrLn (getMessage r)
                                putStrLn "\0"
                                repl r
                        
shell :: Obj -> IO ImpureEvaluation
shell root = do
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


