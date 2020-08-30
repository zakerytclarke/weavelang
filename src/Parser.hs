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
  | Char Float 
  | Number Float 
  | Variable String 
  | Addition Term Term 
  | Subtraction Term Term 
  | Multiplication Term Term 
  | Division Term Term 
  | Exponentiation Term Term 
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
  | Concat Term Term 
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
  | For Term Term String [Term] deriving(Show,Eq,Ord)

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
  show (L s t) = "(λ"++s++". "++(show t)++")"
  show p@(A (A (I "pair") x) y) = "'" ++ showPairHelper p ++ "'"
  show (I "null") = ""
  show (A (I n) a) = show (I n)++" "++(show a)++" "
  show (A s a) = "("++show s++")"++(show a)
  show (I name) = name
  show (C a) = [chr (round a)]
  show (N a) = show a
  show (B 0) = "F"
  show (B 1) = "T"
  show (B (-1)) = "Null"

showPairHelper (A (A (I "pair") (C x)) (C y)) = [chr $ round x] ++ [chr $ round y]
showPairHelper (A (A (I "pair") (C x)) y) = [chr $ round x] ++ showPairHelper y
showPairHelper (A (A (I "pair") x) (C y)) = showPairHelper x ++ [chr $ round y]
showPairHelper (A (A (I "pair") x) y) = showPairHelper x ++ showPairHelper y
showPairHelper y = show y


prgm = space >> (many statement)


statement = 
  --Return Statement
  do {symb "return"; space; a <- expr; space; symb ";"; return (Return a)}
  +++
  --Function Declaration
  do {var<-variableName; space; symb ":="; space; symb "("; args <- variableList; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; return (Assignment var (FunctionDef args ss))}
  +++
  --If-Else Statement
  do {symb "if"; space; symb "("; test <- expr; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; space; symb "else"; symb "{"; space; ss2 <- (many statement); symb "}";space; return (If test ss ss2)}
  +++
  
  --If Statement
  do {symb "if"; space; symb "("; test <- expr; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; return (If test ss [])}
  +++
  --For Statement
  do {symb "for"; space; symb "("; space; var <- variableName; space; symb ","; space; symb "["; start <- expr; space; symb ":"; end <- expr; space; symb "]";  space; symb ")"; space; symb "{"; space; ss <- (many statement); symb "}"; return (For start end var ss)}
  +++
  --Variable Array Assignment
  do {var<-variableName; space; symb "["; space; index <- expr ;symb "]"; space; symb ":="; e <- expr; symb ";"; return (AssignmentArr var index e)}
  +++
  --Variable Assignment
  do {var<-variableName; space; symb ":="; e <- expr; symb ";"; return (Assignment var e)}
  +++  
  ---Expression do
  do {e <- expr; symb ";"; return (Assignment "_" e)}--Assign return value to nothing

 

exprList = sepby expr (symb ",")


expr = addy

funcCall = do { fName <- variableName; space; symb "("; space; args <- exprList; space; symb ")"; return (FunctionCall fName args) } +++ pareny

addy = chainl1 multy op
  where op = do { space; symb "+"; return $ Addition } 
            +++ do { space; symb "-"; return $ Subtraction }
multy = chainl1 expy op
  where op = do { space; symb "*"; return $ Multiplication } 
            +++ do { space; symb "/"; return $ Division }
expy = chainl1 stry op
  where op = do { space; symb "^"; return $ Exponentiation }

stry = chainl1 booly op
  where op = do { space; symb "++"; return $ Concat }

booly = chainl1 funcCall op +++ do {symb "!"; space; a <- booly; return (Not a)}
  where op = do { space; symb "||"; return $ Or } 
          +++ do { space; symb "&&"; return $ And }
          +++ do { space; symb "=="; return $ Equals }
          +++ do { space; symb ">="; return $ GreaterThanEqual }
          +++ do { space; symb "<="; return $ LessThanEqual }
          +++ do { space; symb "!="; return $ NotEquals }
          +++ do { space; symb ">"; return $ GreaterThan }
          +++ do { space; symb "<"; return $ LessThan }


pareny = do {symb "("; a <- expr; symb ")"; return a} +++ value 

value = do { symb "True"; return Tru}+++
         do { symb "False"; return Fls}+++
         do {a <- number; return (Number a)} +++ 
         do { a <- variableName; return (Variable a)}+++
         stringLiteral


variableList = do { a <-variableName; space; symb ","; space; b <- variableList; return ([a]++b)} +++ do {c <- variableName; return [c]}

variableName = do {a<-letter; b<-(many alphanum); return ([a]++b)}+++do{symb "_"; return "_"}


stringLiteral = do {symb "'"; a<-(many stringSymb1); symb "'"; return (mkList (map (\x -> Char (fromIntegral (ord x))) a))}+++
                do {symb "\""; a<-(many stringSymb2); symb "\""; return (mkList (map (\x -> Char (fromIntegral (ord x))) a))}

stringSymb1 = do {symb "\\'"; return '\''}+++do{a<-(sat (/='\''));return a}
stringSymb2 = do {symb "\\\""; return '\"'}+++do{a<-(sat (/='\"'));return a} 


number = do {a<-integer; symb "."; b<-integer; return (read ((show a)++"."++(show b)) ::Float)}+++
         do {a<-integer; return (fromIntegral a)}


mkList [] = Null
mkList (x:xs) = Pair x (mkList xs)



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