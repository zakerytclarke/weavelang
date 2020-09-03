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
  | ArrayAccess WrappedExpression [WrappedExpression]
  | BinaryOp Text WrappedExpression WrappedExpression
  | Not WrappedExpression 
  | Negate WrappedExpression
  | Application WrappedExpression [WrappedExpression]
  | Pair WrappedExpression WrappedExpression
  | Annon [WrappedExpression] [WrappedStatement]
  | EmptyList
  deriving (Show,Eq)


data WrappedExpression = WrappedExpression SourcePos Expression
  | WrappedBinaryOp SourcePos Text WrappedExpression WrappedExpression 
  | WrappedNegate SourcePos WrappedExpression
  | WrappedNot SourcePos WrappedExpression
  deriving (Show,Eq)

-- TODO FIX THIS
{-
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

-}

data Statement = If WrappedExpression [WrappedStatement] [WrappedStatement]
  | For WrappedExpression WrappedExpression WrappedExpression [WrappedStatement]
  | Return WrappedExpression
  | ExpressionStatement WrappedExpression
  | VariableAssignment [WrappedExpression] WrappedExpression
  | FunctionAssignment WrappedExpression [WrappedExpression] [WrappedStatement]
  deriving (Show,Eq)

data WrappedStatement = WrappedStatement SourcePos Statement deriving (Show,Eq)

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
stringLiteral = (char '\"' *> manyTill L.charLiteral (char '\"')) <|> (char '\'' *> manyTill L.charLiteral (char '\''))

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

operatorTable :: [[Operator Parser WrappedExpression]]
operatorTable = 
  [ [ Prefix (WrappedNegate <$> getSourcePos <* symbol "-")
    , Prefix (WrappedNot <$> getSourcePos <* symbol "!")
    ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "^") ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "++")
    , InfixR (WrappedBinaryOp <$> getSourcePos <*> symbol ":")
    ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "*")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "/")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "%")
    ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "+")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "-")
    ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "<=")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol ">=")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "<")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol ">")
    ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "==")
    , InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "!=")
    ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "&&") ]
  , [ InfixL (WrappedBinaryOp <$> getSourcePos <*> symbol "||") ]
  ]

variable :: Parser WrappedExpression
variable = WrappedExpression <$> getSourcePos <*> (Variable <$> (pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)))

annonFunction :: Parser WrappedExpression
annonFunction = WrappedExpression <$> getSourcePos <*> (Annon 
  <$> parens (expression `sepBy` (symbol ",")) 
  <*> braces (many statement))

functionCall :: Parser WrappedExpression
functionCall = WrappedExpression <$> getSourcePos <*> (Application <$> variable <*> parens (expression `sepBy` (symbol ",")))

arrayAccess :: Parser WrappedExpression
arrayAccess = WrappedExpression <$> getSourcePos <*> (ArrayAccess <$> variable <*> some (brackets expression))

listLiteral :: Parser WrappedExpression
listLiteral = do
  pos <- getSourcePos
  as <- brackets (expression `sepBy` (symbol ","))
  return $ (mkList as pos)

mkList :: [WrappedExpression] -> SourcePos -> WrappedExpression
mkList [] pos = WrappedExpression pos EmptyList
mkList (x:xs) pos = WrappedExpression pos (Pair x (mkList xs pos))

constant :: Parser WrappedExpression
constant = WrappedExpression <$> getSourcePos <*> (Const <$> 
  ( (CBool <$> boolean)
    <|> (CInt <$> signedInteger)
    <|> (CFloat <$> signedFloat)
    <|> try (CChar <$> charLiteral)
    <|> (CString <$> stringLiteral)))

term :: Parser WrappedExpression
term = choice 
  [ parens expression
  , constant
  , try annonFunction
  , try functionCall
  , try arrayAccess
  , variable
  , listLiteral
  ]

expression :: Parser WrappedExpression
expression = makeExprParser term operatorTable

statement :: Parser WrappedStatement
statement = ifStatement 
  <|> forStatement 
  <|> returnStatement 
  <|> assignmentStatement
  <|> expressionStatement

ifStatement :: Parser WrappedStatement
ifStatement = WrappedStatement <$> getSourcePos <*> (If
  <$ symbol "if" 
  <*> parens expression 
  <*> braces (many statement)
  <*> (try (symbol "else" *> notFollowedBy (symbol "if") *> braces (many statement))
      <|> (symbol "else" *> (do { s <- ifStatement; return $ [s] }))
      <|> (return [])))

forStatement = do
  pos <- getSourcePos
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
  return $ WrappedStatement pos (For a b c d)

returnStatement = WrappedStatement <$> getSourcePos <*> (Return 
  <$ symbol "return"
  <*> expression 
  <* symbol ";")

expressionStatement = WrappedStatement <$> getSourcePos <*> (ExpressionStatement 
  <$> expression
  <* symbol ";")

assignmentStatement = try functionAssignment <|> try variableAssignment <|> try arrayAssignment

variableAssignment = WrappedStatement <$> getSourcePos <*> (VariableAssignment
  <$> (do { a <- variable; return [a]; })
  <* symbol ":="
  <*> expression
  <* symbol ";")

functionAssignment = WrappedStatement <$> getSourcePos <*> (FunctionAssignment
  <$> variable
  <* symbol ":="
  <*> parens (variable `sepBy` (symbol ","))
  <*> braces (many statement))

arrayAssignment = WrappedStatement <$> getSourcePos <*> (VariableAssignment
  <$> (do { a <- variable; as <- many (brackets expression); return (a : as); })
  <* symbol ":="
  <*> expression
  <* symbol ";")

program :: Parser [WrappedStatement]
program = spaceConsumer >> many statement

parseProgram :: Text -> [WrappedStatement]
parseProgram a = case parse program "" a of
  Left err -> error (errorBundlePretty err)
  Right a -> a

parseFile :: String -> IO [WrappedStatement]
parseFile file = do
  contents <- readFile file
  return $ parseProgram (pack contents)

parseTestFile :: IO [WrappedStatement]
parseTestFile = do
  contents <- readFile "../sample_programs/test.wv"
  return $ parseProgram (pack contents)