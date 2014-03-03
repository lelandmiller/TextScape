module Interpreter.Kernel (kernelFunctions) where

import           Interpreter.Data
import           Interpreter.Eval
import           System.Directory

kernelFunctions :: [(String, Obj)]
kernelFunctions = impureFunctions ++ pureFunctions

-- Impure Functions ---------------------------------------------------

impureFunctions :: [(String, Obj)]
impureFunctions =
        [("openFile", IOFun openFile)
        ,("pwd", IOFun pwd)
        ,("eval", IOFun kerEval)
        ,("cd", IOFun cd)
        ,("writeFile", IOFun kerWriteFile)
        ,("ls", IOFun ls)]

ls :: IOFun
ls root = EitherT $ do
        cwd <- getCurrentDirectory
        buff <- (getDirectoryContents cwd)
        return $ returnMessage (show buff) root

kerEval :: IOFun
kerEval root = do
        Var first <- evalToImpure (getAnonArg 0 root)
        case first of
                "pandoc" -> do
                        Var content <- evalToImpure (getAnonArg 1 root)
                        evalPandoc content root
                x -> eval x root

pwd :: IOFun
pwd root =  EitherT $ do
        c <- getCurrentDirectory
        return $ returnMessage c root

cd :: IOFun
cd root =  do
        Var filename <- evalToImpure (getAnonArg 0 root)
        EitherT $ do
                setCurrentDirectory filename
                return $ returnMessage ("Changed directory to " ++ filename) root

kerWriteFile :: IOFun
kerWriteFile root = do
        Var filename <- evalToImpure (getAnonArg 0 root)
        Var content  <- evalToImpure (getAnonArg 1 root)
        EitherT $ do
                writeFile filename content
                return $ returnMessage ("Wrote data to " ++ filename) root
                
        
openFile :: IOFun
openFile root =  do
        Var filename <- evalToImpure (getAnonArg 0 root)
        EitherT $ openFile' filename root

openFile' :: String -> Obj -> IO Evaluation
openFile' filename root = do
        content <- readFile filename
        return $ returnMessage content root

-- Pure Functions -----------------------------------------------------

pureFunctions :: [(String, Obj)]
pureFunctions =
        [("cat", KerFun cat)
        ,("let", KerFun kerLet)
        ,("listSym", KerFun listSym)
        ,("makeNamespace", KerFun makeNamespace)]

makeNamespace :: KerFun
makeNamespace root = do
        Var name <- getAnonArg 0 root
        insertSymbol name emptyNS root

cat :: KerFun
cat root = do
        args <- getSymbol anonymousArgPath root
        let m = case args of
                List xs -> concat $ foldr (\x acc -> (show x) : acc) [] xs
                _       -> "Improper syntax for cat."
        returnMessage m root

kerLet :: KerFun
kerLet root = do
        Var n <- getAnonArg 0 root
        v <- getAnonArg 1 root
        insertSymbol n v root >>= returnMessage "let success"

listSym :: KerFun
listSym root = returnMessage (printSymbolTree root) root
