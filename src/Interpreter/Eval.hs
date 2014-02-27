module Interpreter.Eval where

-- To access a the first nameless arg use arg._

-- Imports ------------------------------------------------------------
import           Control.Monad
import           Interpreter.Data
import           Interpreter.Parser
import           Control.Applicative
argNS :: String
argNS = "Kernel.Args"

anonymousArgPath :: [Char]
anonymousArgPath = argNS ++ "._"
-- Functions ----------------------------------------------------------
eval :: String -> Obj -> RetObj
eval expression root =
        case parseExpressions expression of
                Right expr -> evalExpressions expr root
                Left  m -> Left (show m)

evalPandoc :: String -> Obj -> RetObj
evalPandoc input root =
        case parsePandoc input of
                Right expr -> evalExpressions expr root
                Left m -> Left (show m)

evalExpressions :: [ParsedText] -> Obj -> RetObj
evalExpressions expressions root = foldl (\acc x -> acc >>= evalExpression x) (Right root) expressions

-- Takes a tsList and evaluates it
evalExpression :: ParsedText -> Obj -> RetObj
evalExpression (ParseList (Atom x:xs)) root = getSymbol x root >>= (\function ->  apply function xs root)
evalExpression _ _ = Left "Evaluation error, expression was not a list."

apply :: Obj -> [ParsedText] -> Obj -> RetObj
apply f@(KerFun _) args root =  applyKerFun f args root
--apply f@(IOFun _)  args root = applyIOFun

-- To apply a function, load save NS arg, add props into it, apply the function, return symboltable with old arg NS
applyKerFun :: Obj -> [ParsedText] -> Obj -> RetObj
applyKerFun (KerFun f) args root = do
        oldArgNS <- getSymbol argNS root
        newNS <- insertSymbol argNS emptyNS root >>= loadArgs args
        f newNS >>= insertSymbol argNS oldArgNS

getArg :: String -> Obj -> RetObj
getArg n = getSymbol (argNS ++ "." ++ n)

getAnonArg :: Int -> Obj -> RetObj
getAnonArg n root = do
        list <- getSymbol anonymousArgPath root
        case list of
                List l -> if (length l) > n
                        then Right (l !! n)
                        else Left "Incorrect parameters."
                _ -> Left "Error loading argument list."



loadArgs :: [ParsedText] -> Obj -> RetObj
loadArgs args root = foldl (\acc x -> acc >>= loadArg x) (Right root) args

loadArg :: ParsedText -> Obj -> RetObj
loadArg (Record (name, Literal x)) root = insertSymbol (argNS ++ "." ++ name) (Var x) root
loadArg (Record (name, Atom x)) root = getSymbol x root >>= (\sym -> insertSymbol (argNS ++ "." ++ name) sym root)
loadArg (Literal x) root = appendToList anonymousArgPath (Var x) root
--loadArg (Record ("_", Literal x)) root
loadArg (Atom x) root = getSymbol x root >>= (\sym -> appendToList anonymousArgPath (sym) root)
loadArg (ParseList xs) root = do
        let oldMessage = getMessage root
        newRoot <- evalExpression (ParseList xs) root
        let newMessage = getMessage newRoot
        returnMessage oldMessage newRoot >>= loadArg (Literal newMessage)


