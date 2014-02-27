module Interpreter.Kernel (kernelFunctions) where

import           Interpreter.Data
import           Interpreter.Eval


kernelFunctions :: [(String, Obj)]
kernelFunctions =
        [("cat", KerFun cat)
        ,("let", KerFun let')
        ,("listSym", KerFun listSym)
        ,("makeNamespace", KerFun makeNamespace)]

{-
openFile :: KerFun
openFile root = do
  name <- getAnonArg 0 root
  let contents = case readFile name of
        IO s -> s
  returnMessage (Var contents) root
-}
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

{-
let' :: KerFun
let' root = do
        Var n <- getArg "n" root
        v <- getArg "v" root
        insertSymbol n v root >>= returnMessage "let success"
-}

let' :: KerFun
let' root = do
        Var n <- getAnonArg 0 root
        v <- getAnonArg 1 root
        insertSymbol n v root >>= returnMessage "let success"

listSym :: KerFun
listSym root = returnMessage (show root) root
