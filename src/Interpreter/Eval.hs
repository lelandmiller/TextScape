module Interpreter.Eval where

-- To access a the first nameless arg use arg._

-- Imports ------------------------------------------------------------
import           Interpreter.Data
import           Interpreter.Parser
import Control.Applicative ((<*>), (<$>), pure)

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
--
evalExpressions :: [ParsedText] -> Obj -> ImpureEvaluation
evalExpressions expressions root = foldl (\acc x -> acc >>= evalExpression x) (return root) expressions

-- Takes a tsList and evaluates it
evalExpression :: ParsedText -> Obj -> ImpureEvaluation
evalExpression (ParseList (Atom x:xs)) root = getSymbolImpure x root >>= (\function ->  apply function xs root)
evalExpression (ParseList (FilledPlaceholder x:xs)) root = apply x xs root
evalExpression _ _ = returnImpureEvaluationError "Evaluation error, expression was not a list."

apply :: Obj -> [ParsedText] -> Obj -> ImpureEvaluation
apply f args root = do
        oldArgNS <-  getSymbolImpure argNS root
        newNS <-  insertSymbolImpure argNS emptyNS root >>= loadArgs args
        case f of
                (KerFun f') -> evalToImpure $ f' newNS >>= insertSymbol argNS oldArgNS
                (IOFun  f') -> f' newNS >>= insertSymbolImpure argNS oldArgNS
                (Var    f') -> evalFunctionString f' newNS >>= insertSymbolImpure argNS oldArgNS
                _           -> returnImpureError "Error, object cannot be interpreted as a function."




-- Kernel functions cannot be used as arguments
evalFunctionString :: String -> Obj -> ImpureEvaluation
evalFunctionString f root = 
        case parseExpressions f of
                Left m -> returnImpureError $ show m
                Right rawParsed -> case fillPlaceholders rawParsed root of
                                        Left m -> returnImpureError $ "Improper argument list: " ++ m
                                        Right filteredParsed -> evalExpressions filteredParsed root
                                                   
fillPlaceholders :: [ParsedText] -> Obj -> Either String [ParsedText]
fillPlaceholders l root = foldr (\x acc -> pure (:) <*> fillParsedText x root <*> acc) (Right []) l

fillParsedText :: ParsedText -> Obj -> Either String ParsedText
fillParsedText t root = case t of
                     ParseList xs           -> pure ParseList <*> (foldr (\x acc -> pure (:) <*> fillParsedText x root <*> acc) (Right []) xs)
                     Record (n, x)          -> fillParsedText x root >>= (\y -> Right $ Record (n, y))
                     AnonymousPlaceholder i -> case getAnonArg i root of
                                                        Right x -> Right $ FilledPlaceholder x
                                                        Left m  -> Left m
                     NamedPlaceholder n     -> case getArg n root of
                                                        Right x -> Right $ FilledPlaceholder x
                                                        Left m  -> Left m
                     x                      -> Right x

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
loadArg (Record (name, FilledPlaceholder x)) root = insertSymbolImpure (argNS ++ "." ++ name) x root
loadArg (FilledPlaceholder x) root = appendToListImpure anonymousArgPath x root
loadArg (Record (name, Literal x)) root = insertSymbolImpure (argNS ++ "." ++ name) (Var x) root
loadArg (Record (name, Atom x)) root = getSymbolImpure x root >>= (\sym -> insertSymbolImpure (argNS ++ "." ++ name) sym root)
loadArg (Literal x) root = appendToListImpure anonymousArgPath (Var x) root
loadArg (Atom x) root = getSymbolImpure x root >>= (\sym -> appendToListImpure anonymousArgPath (sym) root)
loadArg (ParseList xs) root = do
        let oldMessage = getMessage root
        newRoot <- evalExpression (ParseList xs) root
        let newMessage = getMessage newRoot
        returnImpureMessage oldMessage newRoot >>= loadArg (Literal newMessage)
loadArg _ _ = returnImpureError "Expression contains invalid argument."


