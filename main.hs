

import System.IO
import Data.Array

import Data.Char (isDigit, isAlpha)
import System.FilePath (isExtSeparator)

data Token =
    StringToken String |
    IntToken String |
    ParenthesisToken Parenthesis |
    ArithmeticToken Arithemtic |
    BracketsToken Brackets |
    CurlyBracketsToken CurlyBrackets |
    EOF |
    Error

data Parenthesis = OpenParen | ClosedParen
data Arithemtic = Add | Subtract | Multiply | Divide
data Brackets = OpenBracket | ClosedBracket
data CurlyBrackets = OpenCurly | ClosedCurly

data Separator = Space | Newline | Semicolon

isSeparator :: Char -> Bool
--isSeparator s = s == ' ' || s == '\n' || s == ';' || s == ')' || s == '('
isSeparator s = s `elem` " \n;()"


instance Show Token where
    show (StringToken str) = str
    show (IntToken i) = i
    show (ArithmeticToken i) = show i
    show (ParenthesisToken i)= show i
    show EOF = "EOF"
    show Error = "Error"

instance Show Parenthesis where
    show OpenParen = "("
    show ClosedParen = ")"

instance Show Brackets where
    show OpenBracket = "["
    show ClosedBracket = "]"

instance Show CurlyBrackets where
    show OpenCurly = "{"
    show ClosedCurly = "}"

instance Show Arithemtic where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"


isArithmetic :: Char -> Bool
isArithmetic c = c `elem` "+-/*"

isParen :: Char -> Bool
isParen c = c `elem` "()"

lexArithmetic :: String -> [Token]
lexArithmetic (c:cs) = ArithmeticToken ar:getTokens cs
    where ar
            | c == '+' = Add
            | c == '-' = Subtract
            | c == '*' = Multiply
            | c == '/' = Divide


showTokens :: [Token] -> String
showTokens tokens = "["  ++ (join "," tokens) ++ "]" where
    join = (\sep ts -> foldr (\a b -> a ++ sep ++ b) "" (map show ts))


lexNumber :: String -> [Token]
lexNumber (c:cs) = currentToken:restTokens
    where
        currentToken = IntToken $ c:num
        (num, rest) = span isDigit cs
        restTokens = if isSep then getTokens rest else [Error]
        isSep = (rest == "" && True) || isSeparator (head rest) || isArithmetic (head rest)

lexString :: String -> [Token]
lexString (c:cs) = currentToken:restTokens
    where
      currentToken = StringToken $ c:str
      (str,rest) = span (/= '"') cs
      restTokens = if rest == "" then [EOF] else getTokens (tail rest)

lexParen :: String -> [Token]
lexParen (c:cs) = currentToken:restTokens
    where
        currentToken = ParenthesisToken $ if c == '(' then OpenParen else ClosedParen
        restTokens = getTokens cs

getTokens :: String -> [Token]
getTokens "" = [EOF]
getTokens (c:cs)
    | isDigit c = lexNumber (c:cs)
    | isArithmetic c = lexArithmetic (c:cs)
    | c == ' ' = getTokens cs
    | c == '"' = lexString cs
    | isParen c = lexParen (c:cs)
    | otherwise = [Error]


main :: IO ()
main = do
    putStrLn "Enter string:"
    input <- getLine
    putStrLn $ showTokens $ getTokens input
    -- putStrLn $ showTokens $ getTokens "45 \"45\"  2323 23 23  23 32 3 45a"


