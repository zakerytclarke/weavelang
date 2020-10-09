{-# LANGUAGE OverloadedStrings #-}

module Parser (Expression(..),Const(..),Statement(..),parseTestFile,parseFile,parseProgram) where

import Data.Text (Text,pack,unpack)
import Data.Void

import Control.Monad.Combinators.Expr

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Const = CInt Integer
  | CBool Bool
  | CChar Char
  | CFloat Double
  | CString String
  deriving (Eq)

instance Show Const where
  show (CInt a) = show a
  show (CChar c) = show c
  show (CBool c) = show c
  show (CFloat f) = show f
  show (CString s) = s

data Expression = Const Const
  | Variable Text
  | ArrayAccess Expression [Expression]
  | BinaryOp Text Expression Expression
  | Not Expression
  | Negate Expression
  | Application Expression [Expression]
  | Pair Expression Expression
  | Annon [Expression] [Statement]
  | EmptyList
  deriving (Eq)


instance Show Expression where
  show (Const const) = show const
  show (Variable a) = unpack a
  show (BinaryOp op a b) = show a ++ unpack op ++ show b
  show (Not a) = "!" ++ show a
  show (Negate a) = "-" ++ show a
  show p@(Pair (Const (CChar a)) b) = showStringHelper p
  show p@(Pair a b) = "[" ++ showPairHelper p ++ "]"
  show (Application a as) = show a ++ "(" ++ showExpressionListHelper as ++ ")"
  show (ArrayAccess a as) = show a ++ showArrayAccessHelper as
  show EmptyList = ""


showArrayAccessHelper [x] = "[" ++ show x ++ "]"
showArrayAccessHelper (x:xs) = "[" ++ show x ++ "]" ++ showArrayAccessHelper xs

showExpressionListHelper [x] = show x
showExpressionListHelper (x:xs) = show x ++ ", " ++ showExpressionListHelper xs

showStringHelper (Pair (Const (CChar a)) EmptyList) = [a]
showStringHelper (Pair (Const (CChar a)) b) = [a] ++ showStringHelper b
showStringHelper (Pair a b) = show a ++ showStringHelper b

showPairHelper (Pair a EmptyList) = show a
showPairHelper (Pair a b) = show a ++ "," ++ showPairHelper b

data Statement = If Expression [Statement] [Statement]
  | For Expression Expression Expression [Statement]
  | Return Expression
  | ExpressionStatement Expression
  | VariableAssignment [Expression] Expression
  | FunctionAssignment Expression [Expression] [Statement]
  deriving (Show,Eq)

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed (return ()) integer

signedFloat :: Parser Double
signedFloat = L.signed (return ()) float

boolean :: Parser Bool
boolean = do { symbol "True"; return True; }
  <|> do { symbol "False"; return False; }

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ Prefix (Negate <$ symbol "-")
    , Prefix (Not <$ symbol "!")
    ]
  , [ InfixL (BinaryOp <$> symbol "^") ]
  , [ InfixL (BinaryOp <$> symbol "++")
    , InfixR (BinaryOp <$> symbol ":")
    ]
  , [ InfixL (BinaryOp <$> symbol "*")
    , InfixL (BinaryOp <$> symbol "/")
    , InfixL (BinaryOp <$> symbol "%")
    ]
  , [ InfixL (BinaryOp <$> symbol "+")
    , InfixL (BinaryOp <$> symbol "-")
    ]
  , [ InfixL (BinaryOp <$> symbol "<=")
    , InfixL (BinaryOp <$> symbol ">=")
    , InfixL (BinaryOp <$> symbol "<")
    , InfixL (BinaryOp <$> symbol ">")
    ]
  , [ InfixL (BinaryOp <$> symbol "==")
    , InfixL (BinaryOp <$> symbol "!=")
    ]
  , [ InfixL (BinaryOp <$> symbol "&&") ]
  , [ InfixL (BinaryOp <$> symbol "||") ]
  , [ InfixR (BinaryOp <$> symbol ":=") ]
  ]

variable :: Parser Expression
variable = Variable <$> (pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar))

annonFunction :: Parser Expression
annonFunction = Annon
  <$> parens (expression `sepBy` (symbol ","))
  <*> braces (many statement)

functionCall :: Parser Expression
functionCall = Application <$> variable <*> parens (expression `sepBy` (symbol ","))

arrayAccess :: Parser Expression
arrayAccess = ArrayAccess <$> variable <*> some (brackets expression)

listLiteral :: Parser Expression
listLiteral = do
  as <- brackets (expression `sepBy` (symbol ","))
  return $ mkList as

mkList :: [Expression] -> Expression
mkList [] = EmptyList
mkList (x:xs) = Pair x (mkList xs)

constant :: Parser Expression
constant = Const <$>
  ( (CBool <$> boolean)
    <|> (CInt <$> signedInteger)
    <|> (CFloat <$> signedFloat)
    <|> (CChar <$> charLiteral)
    <|> (CString <$> stringLiteral))

term :: Parser Expression
term = choice
  [ parens expression
  , constant
  , try annonFunction
  , try functionCall
  , try arrayAccess
  , variable
  , listLiteral
  ]

expression :: Parser Expression
expression = makeExprParser term operatorTable

statement :: Parser Statement
statement = ifStatement
  <|> forStatement
  <|> returnStatement
  <|> assignmentStatement
  <|> expressionStatement

ifStatement = If
  <$ symbol "if"
     <- getOffset
  <*> parens expression
  <*> braces (many statement)
  <*> (try (symbol "else" *> notFollowedBy (symbol "if") *> braces (many statement))
      <|> (symbol "else" *> (do { s <- ifStatement; return $ [s] }))
      <|> (return []))

forStatement = do
  symbol "for"
  symbol "("
  a <- variable
  symbol ","
  symbol "["
  b <- expression
  symbol ".."
  c <- expression
  symbol "]"
  symbol ")"
  d <- braces (many statement)
  return $ For a b c d

returnStatement = Return
  <$ symbol "return"
  <*> expression
  <* symbol ";"

expressionStatement = ExpressionStatement
  <$> expression
  <* symbol ";"

assignmentStatement = try functionAssignment <|> try variableAssignment <|> try arrayAssignment

variableAssignment = VariableAssignment
  <$> (do { a <- variable; return [a]; })
  <* symbol ":="
  <*> expression
  <* symbol ";"

functionAssignment = FunctionAssignment
  <$> variable
  <* symbol ":="
  <*> parens (variable `sepBy` (symbol ","))
  <*> braces (many statement)

arrayAssignment = VariableAssignment
  <$> (do { a <- variable; as <- many (brackets expression); return (a : as); })
  <* symbol ":="
  <*> expression
  <* symbol ";"

program :: Parser [Statement]
program = spaceConsumer >> many statement

parseProgram :: Text -> [Statement]
parseProgram a = case parse program "" a of
  Left err -> error (errorBundlePretty err)
  Right a -> a

parseFile :: String -> IO [Statement]
parseFile file = do
  contents <- readFile file
  return $ parseProgram (pack contents)

parseTestFile :: IO [Statement]
parseTestFile = do
  contents <- readFile "../sample_programs/test.wv"
  return $ parseProgram (pack contents)
