module Interpreter.Kernel (kernelFunctions) where

import           Interpreter.Data
import           Interpreter.Eval
import           System.Directory

kernelFunctions :: [(String, Obj)]
kernelFunctions =
        [("cat", KerFun cat)
        ,("let", KerFun let')
        ,("listSym", KerFun listSym)
        ,("makeNamespace", KerFun makeNamespace)
        ,("openFile", IOFun openFile)
        ,("pwd", IOFun pwd)]

-- Impure Functions ---------------------------------------------------

pwd :: IOFun
pwd root =  EitherT $ do
        c <- getCurrentDirectory
        return $ returnMessage c root

openFile :: IOFun
openFile root =  do
        Var filename <- evalToImpure (getAnonArg 0 root)
        EitherT $ openFile' filename root

openFile' :: String -> Obj -> IO Evaluation
openFile' filename root = do
        content <- readFile filename
        return $ returnMessage content root

-- Pure Functions -----------------------------------------------------

makeNamespace :: KerFun
makeNamespace root = do
        Var name <- getAnonArg 0 root
        insertSymbol name emptyNS root

cat :: KerFun
cat root = do
        args <- getSymbol anonymousArgPath root
        let m = case args of
                List xs -> unwords $ foldr (\x acc -> (show x) : acc) [] xs
                _       -> "Improper syntax for cat."
        returnMessage m root

let' :: KerFun
let' root = do
        Var n <- getAnonArg 0 root
        v <- getAnonArg 1 root
        insertSymbol n v root >>= returnMessage "let success"

listSym :: KerFun
listSym root = returnMessage (show root) root
