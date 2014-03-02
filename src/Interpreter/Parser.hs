module Interpreter.Parser where

-- TODO: Add comments, multiple lists, multiple lines
      -- numbers in symbols

import           Control.Monad
import           Data.List
import           Text.ParserCombinators.Parsec

type RecordName = String

type Record = (RecordName, ParsedText)

data ParsedText = Atom String
                | Literal String
                | ParseList [ParsedText]
                | Record (RecordName, ParsedText)
                | AnonymousPlaceholder Int
                | NamedPlaceholder String



instance Show ParsedText where
        show (Atom x) = x
        show (Literal x) = '\"' : x ++ "\""
        show (ParseList xs) = concat $ intersperse "\n  " ("[" : (foldr (\x acc -> (show x) : acc) [] xs) ++ ["]"])
        show (Record (name, val)) = "(" ++ name ++ ", " ++ (show val) ++ ")"
        show (AnonymousPlaceholder x) = '@' : (show x)
        show (NamedPlaceholder x) = '$' : x

parseAnonPlaceholder :: Parser ParsedText
parseAnonPlaceholder = do
        _ <- char '@'
        index <- many1 digit
        return $ AnonymousPlaceholder (read index :: Int)

parseNamedPlaceholder :: Parser ParsedText
parseNamedPlaceholder = do
        _ <- char '$'
        name <- many1 (letter <|> digit)
        return $ NamedPlaceholder name

parsePlaceholder :: Parser ParsedText
parsePlaceholder = try (parseAnonPlaceholder <|> parseNamedPlaceholder)

parseRecord :: Parser ParsedText
parseRecord = do
  name  <- many1 letter
  _     <- char ':'
  value <- parseAny
  return $ Record (name, value)

parseList :: Parser ParsedText
parseList = do
        _ <- char '('
        x <- sepBy parseAny (many1 space)
        _ <- char ')'
        return $ ParseList x

parseSymbol :: Parser ParsedText
parseSymbol = do
  symbol <- many1 (letter <|> oneOf "." <|> digit)
  return (Atom symbol)

{-
parseLiteral :: Parser ParsedText
parseLiteral = do
        delim <- char '\'' <|> char '\"'
        s <- many $ try parseEscapes <|> (noneOf [delim])
        _ <- char delim
        return $ Literal s

parseEscapes :: Parser Char
parseEscapes = do
        x <- try (string "\\\"") <|> (string "\\'") <|> (string "\\\\")
        case x of
                "\\'" -> return '\''
                "\\\\" -> return '\\'
                "\\\"" -> return '\"'
-}
parseLiteral :: Parser ParsedText
parseLiteral = do
        delim <- char '/'
        s <- many $ try parseEscapes <|> (noneOf [delim])
        _ <- char delim
        return $ Literal s

parseEscapes :: Parser Char
parseEscapes = do
        x <- try (string "//") <|> (string "\\n") <|> (string "\\t")
        case x of
                "//" -> return '/'
                "\\n" -> return '\n'
                "\\t" -> return '\t'
                _     -> return ' ' -- To stop warnings
                


parseAny :: Parser ParsedText
parseAny = parseLiteral
       <|> parseList
       <|> parsePlaceholder
       <|> (try parseRecord <|> parseSymbol)

parseExpression :: String -> Either ParseError ParsedText
parseExpression = parse parseList "textscape"

parseSpacedExpression :: Parser ParsedText
parseSpacedExpression = do 
        spaces
        x <- parseList
        spaces
        return x

expressionsParser :: Parser [ParsedText]
expressionsParser = do
        spaces
        x <- sepBy parseList spaces
        spaces
        return x

parseExpressions :: String -> Either ParseError [ParsedText]
parseExpressions = parse expressionsParser "textscape"

{-
parsePandocBlock :: Parser [ParsedText]
parsePandocBlock = do
        _ <- string "```ts"
        _ <- spaces
        l <- sepBy parseList spaces
        _ <- spaces
        _ <- string "```"
        return l
-}

parsePandocBlock :: Parser String
parsePandocBlock = do
        _ <- string "```ts"
        _ <- spaces
        l <- manyTill anyChar (try (string "```"))
        --l <- many (noneOf "`") 
        --_ <- spaces
        --_ <- string "```"
        return l

parsePandocDoc = do
        _ <- manyTill anyChar (try (string "```ts"))
        x <- manyTill parseSpacedExpression (try (string "```"))
        --string "```"
        return $ x
        
        {-simpleComment   = do{ string "<!--"
                    ; manyTill anyChar (try (string "-->"))
                    }-}

--parsePandoc :: String -> Either ParseError String --[ParsedText]
parsePandoc input = liftM concat $ parse (many (try parsePandocDoc)) "ts" input --liftM concat $ parse parsePandocDoc "textscape" input




