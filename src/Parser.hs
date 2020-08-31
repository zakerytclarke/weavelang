module Parser (parsePrgm,Term(..),Type(..),EvalTerm(..)) where

import Parselib
import Data.Char


data Type =  Boolean | Int | Numeric | Character | Func Type Type | Unknown | UnknownArg String | List Type | IO deriving(Eq)

instance Show Type where
  show Boolean = "Bool"
  show Numeric = "Num"
  show Character = "Char"
  show f@(Func x y) = "(" ++ showFuncHelper f ++ ")"
  show Unknown = "Unknown"
  show (UnknownArg a) = "UnknownArg " ++ show a
  show (List Character) = "String"
  show (List a) = "List " ++ show a
  show IO = "IO"

showFuncHelper (Func x y) = showFuncHelper x ++ "->" ++ showFuncHelper y
showFuncHelper y = show y

data Term = FunctionDef [String] [Term] 
  | Tru 
  | Fls 
  | Null 
  | EmptyList
  | Char Float 
  | Number Float 
  | Variable String 
  | VariableArr String Term
  | Addition Term Term 
  | Subtraction Term Term 
  | Multiplication Term Term 
  | Division Term Term 
  | Exponentiation Term Term
  | Mod Term Term
  | Not Term 
  | And Term Term 
  | Or Term Term 
  | NotEquals Term Term
  | Equals Term Term 
  | GreaterThan Term Term 
  | GreaterThanEqual Term Term 
  | LessThanEqual Term Term  
  | LessThan Term Term 
  | Pair Term Term 
  | Append Term Term 
  | Head Term
  | Tail Term 
  | Map Term Term 
  | Assignment String Term 
  | AssignmentArr String Term Term 
  | FunctionCall String [Term] 
  | Lambda String Term 
  | App Term [Term] 
  | FuncPtr String 
  | Return Term 
  | If Term [Term] [Term] 
  | EvalIf Term Term Term 
  | For String Term Term [Term]
  | EvalFor Term Term Term Term Term deriving(Show,Eq,Ord)

{-
instance Show Term where
  show Null = ""
  show (Char num) = [chr $ round num]
  show (Pair x y) = (show x)++(show y)
-}


--Evaluation Type
data EvalTerm = L String EvalTerm --Lambda Abstraction
  | A EvalTerm EvalTerm  --Application
  | AB EvalTerm EvalTerm --Built in Function 
  | I String --Variable & Function references
  | N Float --Number 
  | C Float --Character
  | B Float deriving(Eq) -- Boolean


instance Show EvalTerm where
  show (L s t) = "(λ"++s++". "++(show t)++") "
  show p@(AB (AB (I "pair") (C x)) y) = showStringHelper p
  show p@(AB (AB (I "pair") x) y) = "["++(showPairHelper p) ++ "]"
  show (I "null") = ""
  show (I "[]") = ""
  show (A (I n) a) = show (I n)++" "++(show a)
  show (A s a) = "("++show s++")"++(show a)
  show (I ('~':name)) = name
  show (I name) = name
  show (C 0) = ""
  show (C a) = [chr (round a)]
  show (N a) = show a
  show (B 0) = "F"
  show (B 1) = "T"
  show (B (-1)) = ""


showStringHelper (AB (AB (I "pair") x) (B (-1))) = show x
showStringHelper (AB (AB (I "pair") x) y) = show x ++ showStringHelper y
showStringHelper y = show y

showPairHelper :: EvalTerm -> String
showPairHelper (AB (AB (I "pair") x) (I "[]")) = show x
showPairHelper (AB (AB (I "pair") x) (B (-1))) = show x
showPairHelper (AB (AB (I "pair") (I "[]")) _) = "..."
showPairHelper (AB (AB (I "pair") x) y) = (show x) ++ ", " ++ (showPairHelper y)
showPairHelper y = show y


prgm :: Parser [Term]
prgm = space >> (many statement)

statement :: Parser Term
statement = 
  --Return Statement
  do {symb "return"; space; a <- expr; space; symb ";"; return (Return a)}
  +++
  --Function Declaration
  do {var<-variableName; space; symb ":="; space; symb "("; args <- variableList; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; return (Assignment var (FunctionDef args ss))}
  +++
  ifStatementParser
  +++
  --For Statement
  do {symb "for"; space; symb "("; space; var <- variableName; space; symb ","; symb "["; start <- number; space; symb ":"; end <- number; space; symb "]";  space; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; return (For var (Number start) (Number end) ss)}
  +++
  --Variable Array Assignment
  do {var<-variableName; space; symb "["; space; index <- expr ;symb "]"; space; symb ":="; e <- expr; symb ";"; return (AssignmentArr var index e)}
  +++
  --Variable Assignment
  do {var<-variableName; space; symb ":="; e <- expr; symb ";"; return (Assignment var e)}
  +++  
  ---Expression do
  do {e <- expr; symb ";"; return (Assignment "_" e)}--Assign return value to nothing

ifStatementParser :: Parser Term
ifStatementParser = do {symb "if"; space; symb "("; test <- expr; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; space; symb "else"; symb "{"; space; ss2 <- (many statement); symb "}";space; return (If test ss ss2)} 
  +++ do {symb "if"; space; symb "("; test <- expr; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; space; symb "else"; ss2 <- ifStatementParser; space; return (If test ss [ss2])} 
  +++ do {symb "if"; space; symb "("; test <- expr; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; return (If test ss [])}

exprList :: Parser [Term]
exprList = sepby expr (symb ",")

expr = logicalOr

logicalOr = chainl1 logicalAnd op
  where op = do { space; symb "||"; return Or }

logicalAnd = chainl1 equality op
  where op = do { space; symb "&&"; return And }

equality = chainl1 relational op
  where op = do { space; symb "=="; return Equals }
            +++ do { space; symb "!="; return NotEquals }

relational = chainl1 addy op
  where op = do { space; symb ">="; return $ GreaterThanEqual }
          +++ do { space; symb "<="; return $ LessThanEqual }
          +++ do { space; symb ">"; return $ GreaterThan }
          +++ do { space; symb "<"; return $ LessThan }

addy = chainl1 multy op
  where op = do { space; symb "+"; return $ Addition } 
            +++ do { space; symb "-"; return $ Subtraction }

multy = chainl1 expy op
  where op = do { space; symb "*"; return $ Multiplication } 
            +++ do { space; symb "/"; return $ Division }
            +++ do { space; symb "%"; return $ Mod }

expy = chainl1 stry op
  where op = do { space; symb "^"; return $ Exponentiation }

stry = chainl1 cons op
  where op = do { space; symb "++"; return $ Append }

cons = chainr1 noty op
  where op = do { space; symb ":"; return $ Pair }

noty = do {symb "!"; space; a <- funcCall; return (Not a)}
  +++ funcCall


funcCall = do { fName <- variableName; space; symb "("; space; args <- exprList; space; symb ")"; return (FunctionCall fName args) } 
  +++ do { aName <- variableName; space; symb "["; i <- expr; symb "]"; return (VariableArr aName i) }
  +++ pareny

pareny = do {symb "("; a <- expr; symb ")"; return a} +++ value 

value = do { symb "True"; return Tru}+++
         do { symb "False"; return Fls}+++
         do {a <- number; return (Number a)} +++ 
         do { a <- variableName; return (Variable a)}+++
         listLiteral +++
         stringLiteral


variableList = do { a <-variableName; space; symb ","; space; b <- variableList; return ([a]++b)} +++ do {c <- variableName; return [c]}

variableName = do {a<-letter; b<-(many alphanum); return ([a]++b)}+++do{symb "_"; return "_"}

listLiteral = do { space; symb "["; vals <- exprList; symb "]"; return $ mkList vals } 
  +++ do { space; symb "["; symb "]"; return EmptyList }

stringLiteral = do {space; string "'"; a<-(many stringSymb1); symb "'"; return (mkString (map (\x -> Char (fromIntegral (ord x))) a))}+++
                do {space; string "\""; a<-(many stringSymb2); symb "\""; return (mkString (map (\x -> Char (fromIntegral (ord x))) a))}

stringSymb1 = do {symb "\\'"; return '\''}+++do{a<-(sat (/='\''));return a}
stringSymb2 = do {symb "\\\""; return '\"'}+++do{a<-(sat (/='\"'));return a} 


number = do {a<-integer; symb "."; b<-integer; return (read ((show a)++"."++(show b)) ::Float)}+++
         do {a<-integer; return (fromIntegral a)}

mkList [] = EmptyList
mkList (x:xs) = Pair x (mkList xs)


mkString [] = EmptyList
mkString (x:xs) = Pair x (mkString xs)


parsePrgm text = 
  case (parse prgm text) of
    --Worked Completely; Return AST
    [(ast,"")] -> ast
    --Worked Partially
    [(ast,remainder)] -> 
      error ("Failed to Parse at "++(show failedRow)++":"++(show failedCol)++"\n"++errorTxt++"\n"++(arrows (length errorTxt)))
      where failedAt = countNL (reverse (take (length remainder) (reverse text))) 0 0
            failedRow = fst failedAt
            failedCol = snd failedAt
            tLen = length text
            countNL (t:ts) c tot = if t=='\n'
                                     then (countNL ts 0 (tot+1))
                                     else (countNL ts (c+1) tot)
            countNL [] c tot =  (tot,c)
            getErrorTxt [] = "" 
            getErrorTxt ('\n':xs) = ""
            getErrorTxt (x:xs) = x:(getErrorTxt xs)
            errorTxt = getErrorTxt remainder
            arrows 0 = "^"
            arrows n = "^"++(arrows (n-1))