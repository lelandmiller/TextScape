module Interpreter.Eval where

-- To access a the first nameless arg use arg._

-- Imports ------------------------------------------------------------
import           Interpreter.Data
import           Interpreter.Parser
argNS :: String
argNS = "Kernel.Args"

anonymousArgPath :: [Char]
anonymousArgPath = argNS ++ "._"
-- Functions ----------------------------------------------------------
eval :: String -> Obj -> ImpureEvaluation
eval expression root =
        case parseExpressions expression of
                Right expr -> evalExpressions expr root
                Left  m -> returnImpureEvaluationError (show m)

evalPandoc :: String -> Obj -> ImpureEvaluation
evalPandoc input root =
        case parsePandoc input of
                Right expr -> evalExpressions expr root
                Left m -> returnImpureEvaluationError (show m)

evalExpressions :: [ParsedText] -> Obj -> ImpureEvaluation
evalExpressions expressions root = foldl (\acc x -> acc >>= evalExpression x) (return root) expressions

-- Takes a tsList and evaluates it
evalExpression :: ParsedText -> Obj -> ImpureEvaluation
evalExpression (ParseList (Atom x:xs)) root = getSymbolImpure x root >>= (\function ->  apply function xs root)
evalExpression _ _ = returnImpureEvaluationError "Evaluation error, expression was not a list."

apply :: Obj -> [ParsedText] -> Obj -> ImpureEvaluation
apply f args root = do
        oldArgNS <-  getSymbolImpure argNS root
        newNS <-  insertSymbolImpure argNS emptyNS root >>= loadArgs args
        case f of
                (KerFun f) -> evalToImpure $ f newNS >>= insertSymbol argNS oldArgNS
                (IOFun f) ->  f newNS >>= insertSymbolImpure argNS oldArgNS

getArg :: String -> Obj -> Evaluation
getArg n = getSymbol (argNS ++ n)

getAnonArg :: Int -> Obj -> Evaluation
getAnonArg n root = do
        list <- getSymbol anonymousArgPath root
        case list of
                List l -> if (length l) > n
                        then return (l !! n)
                        else returnEvaluationError "Incorrect parameters."
                _ -> returnEvaluationError "Error loading argument list."

loadArgs :: [ParsedText] -> Obj -> ImpureEvaluation
loadArgs args root = foldl (\acc x -> acc >>= loadArg x) (return root) args

loadArg :: ParsedText -> Obj -> ImpureEvaluation
loadArg (Record (name, Literal x)) root = insertSymbolImpure (argNS ++ "." ++ name) (Var x) root
loadArg (Record (name, Atom x)) root = getSymbolImpure x root >>= (\sym -> insertSymbolImpure (argNS ++ "." ++ name) sym root)
loadArg (Literal x) root = appendToListImpure anonymousArgPath (Var x) root
--loadArg (Record ("_", Literal x)) root
loadArg (Atom x) root = getSymbolImpure x root >>= (\sym -> appendToListImpure anonymousArgPath (sym) root)
loadArg (ParseList xs) root = do
        let oldMessage = getMessage root
        newRoot <- evalExpression (ParseList xs) root
        let newMessage = getMessage newRoot
        returnImpureMessage oldMessage newRoot >>= loadArg (Literal newMessage)


