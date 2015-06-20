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
symbol = oneOf "#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found: " ++ case val of
                                               Atom a -> "atom " ++ a
                                               Bool b -> "bool " ++ show b
                                               String s -> "string " ++ s
                                               Number n -> "number " ++ show n
                                               _ -> "something"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (parseEscaped <|> (noneOf "\""))
    char '"'
    return $ String x

parseEscaped :: Parser Char
parseEscaped = char '\\' >> oneOf ['n', 'r', 't', '\\', '"']

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDec <|> parseHex <|> parseOct <|> parseBin

parseHex :: Parser LispVal
parseHex = string "#h" >> many1 digit >>= return . Number . fst . head . readHex

parseOct :: Parser LispVal
parseOct = string "#o" >> many1 digit >>= return . Number . fst . head . readOct

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

parseBin :: Parser LispVal
parseBin = string "#b" >> many1 digit >>= return . Number . fst . head . readBin

parseDec :: Parser LispVal
parseDec = optional (string "#d") >> many1 digit >>= return . Number . fst . head . readDec
