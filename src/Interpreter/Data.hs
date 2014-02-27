module Interpreter.Data
( Obj (..)
, emptyNS
, insertSymbol
, getSymbol
, removeSymbol
, importSymbols
, KerFun
, RetObj
, returnMessage
, getMessage
, appendToList) where

-- Imports ------------------------------------------------------------
import qualified Data.Map         as Map
import           Interpreter.Util


-- Data Types ---------------------------------------------------------
type SymbolName = String
type ErrorMessage = String

type RetObj = Either ErrorMessage Obj

type Buf = String

type KerFun = (Obj -> RetObj)
type IOFun  = (Obj -> IO RetObj)
type NS = (Map.Map SymbolName Obj)

data Obj = Var String
         | NS NS
         | Buf Buf
         | List [Obj]
         | KerFun KerFun
         | IOFun IOFun

instance Show Obj where
        show (Var s) = s
        show (NS s) = Map.foldrWithKey (\k x ks -> k ++ ":(" ++ (show x) ++ ") " ++ ks) [] s
        show (Buf _) = ""
        show (KerFun _) = "Kernel Function"
        show (List s) = show s --"[" ++ (foldr (\x ks -> (show x) ++ ks) [] s) ++ "]"

messagePath = "Kernel.message"
-- Functions ----------------------------------------------------------

returnMessage :: String -> Obj -> RetObj
returnMessage m root = insertSymbol messagePath (Var m) root

getMessage :: Obj -> String
getMessage root = case getSymbol messagePath root of
        Left m -> m
        Right (Var m) -> m
        _ -> "Error retrieving message"

-- TODO: Need to add a does object function and fix this, currently relies on failure
-- of lookup to insert new array
appendToList :: String -> Obj -> Obj -> RetObj
appendToList name object root =
        case getSymbol name root of
                Right (List l) -> insertSymbol name (List (l ++ [object])) root
                _ -> insertSymbol name (List []) root >>= appendToList name object


emptyNS :: Obj
emptyNS = NS (Map.empty)

{- | The function 'insertSymbol' takes a 'String' with the fully-qualified
     name of the new symbol, the symbol and its value of type 'Obj', and
     the root of type 'Obj' which should be a 'NS' and returns and error
     message or the root with the symbol added.

insertSymbol :: String -> Obj -> Obj -> RetObj
insertSymbol name value root =
    case newRoot of
        Just r -> Right r
        Nothing -> Left "Error in insertSymbol'."
    where namel = splitString name '.'
          newRoot = insertSymbol' namel value root
-}
insertSymbol :: String -> Obj -> Obj -> RetObj
insertSymbol name value root =
    case newRoot of
        Just r -> Right r
        Nothing -> Left "Error in insertSymbol."
    where namel = splitString name '.'
          newRoot = modifySymbolEntry (Map.insert) namel value root

removeSymbol :: String -> Obj -> RetObj
removeSymbol name root =
    case newRoot of
        Just r -> Right r
        Nothing -> Left "Error in insertSymbol'."
    where namel = splitString name '.'
          newRoot = modifySymbolEntry (\symname _ ns -> Map.delete symname ns) namel emptyNS root

{- | Takes a fully-qualified pathname as 'String' and a root 'Obj' which
     should be a 'NS' and returns
-}
getSymbol :: String -> Obj -> RetObj
getSymbol name root = getSymbol' (splitString name '.') root


importSymbols :: [(String, Obj)] -> Obj -> RetObj
importSymbols xs root = foldr (\(name, val) acc  -> acc >>= insertSymbol name val) (Right root) xs


-- Internal Functions -------------------------------------------------

{- | f is a function that takes the name of a symbol, the value, and its direct parent namespace and returns that
     parent namespace with some modification.
-}

modifySymbolEntry :: (SymbolName -> Obj -> NS -> NS) -> [String] -> Obj -> Obj -> Maybe Obj
modifySymbolEntry f (k:[]) v (NS n) = Just (NS $ f k v n) -- insert the object into the current NS
modifySymbolEntry f (k:ks) v (NS n) =
        fmap (NS . (\newNS -> Map.insert k (newNS) n)) newMaybeNS
        where newMaybeNS = (Map.lookup k n) >>= (modifySymbolEntry f ks v)
modifySymbolEntry _ _ _ _ = Nothing

{-
--insertSymbol name value root =
-- get next value in name
-- insertSymbol into that value
-- insertValue into current NS
insertSymbol' :: [String] -> Obj -> Obj -> Maybe Obj
insertSymbol' (k:[]) v (NS n) = Just (NS $ Map.insert k v n) -- insert the object into the current NS
insertSymbol' (k:ks) v (NS n) =
        fmap (NS . (\newNS -> Map.insert k (newNS) n)) newMaybeNS
        where newMaybeNS = (Map.lookup k n) >>= (insertSymbol' ks v)
insertSymbol' _ _ _ = Nothing
-}


getSymbol' :: [String] -> Obj -> RetObj
getSymbol' []     root = return root
getSymbol' (k:[]) root = lookupObj k root
getSymbol' (k:ks) root = lookupObj k root >>= (\x -> getSymbol' ks x)

{- | Returns the entry given as a 'String' is it is a child of the given namespace
     'Obj'. Otherwise returns an error message.
-}
lookupObj :: String -> Obj -> RetObj
lookupObj name (NS space) =
    case result of
      Just ret    -> Right ret
      Nothing     -> Left ("Object " ++ name ++ " not found.")
    where result = Map.lookup name space
lookupObj _ _ = Left "Lookup attempted on a non-namespace object."


