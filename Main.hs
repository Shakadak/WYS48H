module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char (digitToInt)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args!!0))

symbol :: Parser Char
symbol = oneOf "$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool deriving Show

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseDec <|> parseSpecial

parseSpecial :: Parser LispVal
parseSpecial = char '#' >> (parseBool <|> {-parseCharacter <|>-} parseNumber)

parseBool :: Parser LispVal
parseBool = do
                c <- oneOf "tf"
                return $ case c of
                    't' -> Bool True
                    'f' -> Bool False

{-parseCharacter :: Parser Lispval

parseCharacter = do
                    char '\'
                    return $ try (any)-}

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (parseEscaped <|> (noneOf "\""))
    char '"'
    return $ String x

parseEscaped :: Parser Char
parseEscaped = do
                char '\\'
                c <- oneOf "nrt\\\""
                return $ case c of
                            'n' -> '\n'
                            'r' -> '\r'
                            't' -> '\t'
                            x   -> x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    c <- oneOf "bodx"
    case c of
      'b' -> parseBin
      'o' -> parseOct
      'd' -> parseDec
      'x' -> parseHex

parseHex :: Parser LispVal
parseHex = many1 (digit <|> oneOf "abcdefABCDEF") >>= return . Number . fst . head . readHex

parseOct :: Parser LispVal
parseOct = many1 digit >>= return . Number . fst . head . readOct

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

parseBin :: Parser LispVal
parseBin = many1 digit >>= return . Number . fst . head . readBin

parseDec :: Parser LispVal
parseDec = many1 digit >>= return . Number . fst . head . readDec
