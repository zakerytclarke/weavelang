{-# LANGUAGE OverloadedStrings #-}

module Transform (Term(..), transform, prettyPrintStatementTrans) where

import Parser
import Data.Text (Text,pack,unpack, append)

data Term = App Term Term
  | Lambda Term Term
  | Var Text
  | BuiltIn Text
  | Null
  | Value Const
  deriving (Show,Eq)

getVar name [] = error ("Undefined variable: "++ show name)
getVar name (f:fs) = 
  if (fst f)==name
    then (snd f)
    else getVar name fs

prettyPrintStatementTrans :: Term -> String
prettyPrintStatementTrans (Value a) = show a
prettyPrintStatementTrans Null = ""
prettyPrintStatementTrans (Var name) = unpack name
prettyPrintStatementTrans (BuiltIn a) = unpack a
prettyPrintStatementTrans (Lambda a sexpr)  = "(l." ++ prettyPrintStatementTrans a ++ " " ++ prettyPrintStatementTrans sexpr ++ ")"
prettyPrintStatementTrans a@(App (App (BuiltIn "pair") (Value (CChar x))) y) = showStringHelper a 
prettyPrintStatementTrans a@(App (App (BuiltIn "pair") x) y) = "[" ++ showPairHelper a ++ "]"
prettyPrintStatementTrans (App sexpr arg) = "(" ++ prettyPrintStatementTrans sexpr ++ " " ++ prettyPrintStatementTrans arg ++ ")"
{-
instance Show Term where
  show (Value a) = show a
  show Null = ""
  show (Var a) = (unpack a)
  show (Lambda a sexpr) = "(l." ++ show a ++ " " ++ show sexpr ++ ")"
  show a@(App (App (Var "pair") (Value (CChar x))) y) = showStringHelper a
  show a@(App (App (Var "pair") x) y) = "[" ++ showPairHelper a ++ "]"
  show (App sexpr arg) = "(" ++ show sexpr ++ " " ++ show arg ++ ")"
-}
showStringHelper (App (App (BuiltIn "pair") (Value (CChar x))) Null) = [x]
showStringHelper (App (App (BuiltIn "pair") (Value (CChar x))) y) = [x] ++ showStringHelper y 
showStringHelper (App (App (BuiltIn "pair") x) y) = prettyPrintStatementTrans x ++ showStringHelper y 

showPairHelper (App (App (BuiltIn "pair") x) Null) = prettyPrintStatementTrans x 
showPairHelper (App (App (BuiltIn "pair") Null) y) = "  " ++ "," ++ showPairHelper y 
showPairHelper (App (App (BuiltIn "pair") x) y) = prettyPrintStatementTrans x ++ "," ++ showPairHelper y 

nthDim :: Integer -> Text
nthDim 1 = "1st"
nthDim 2 = "2nd"
nthDim 3 = "3rd"
nthDim n = (pack $ show n) `append` "nth"

arrLambda :: Integer -> Text -> Text -> Term -> Term
arrLambda 0 name func end = (Lambda (Var ("~Arr")) (Lambda (Var func) end))
arrLambda n name func end = (Lambda (Var (append "~" (nthDim n))) (arrLambda (n - 1) (append (append "~" (nthDim n)) name) func end))

forLoop var start end body rest = 
  (App 
    (App 
      (App 
        (App
          (Lambda (Var "~forStart") 
            (Lambda (Var "~forEnd") 
              (Lambda (Var "~forVar") 
                (Lambda (Var "~forBody") 
                  (Lambda (Var "~for") (transform rest))
                )
              )
            )
          ) (transformE start) 
        ) (transformE end)
      ) (transformE var)
    ) (transform body)
  )

transformE :: Expression -> Term
transformE e =
  case e of
    Const (CString str) -> mkStringList str
    Const c -> Value c 
    Variable c -> case c of
      "pair" -> BuiltIn c
      "append" -> BuiltIn c
      "length" -> BuiltIn c
      "head" -> BuiltIn c
      "tail" -> BuiltIn c
      "print" -> BuiltIn c
      "input" -> BuiltIn c
      otherwise -> Var c
    ArrayAccess a bs -> (arrayAccessCreator (a:bs) 0)
    BinaryOp ":" a b -> App (App (BuiltIn "pair") (transformE a)) (transformE b)
    BinaryOp "++" a b -> App (App (BuiltIn "append") (transformE a)) (transformE b)
    BinaryOp op a b -> App (App (BuiltIn op) (transformE a)) (transformE b)
    Not e -> App (BuiltIn "!") (transformE e)
    Negate e -> App (BuiltIn "-") (transformE e)
    Application (Variable c) args -> appHelper c (reverse args)
    Pair a b -> (App (App (BuiltIn "pair") (transformE a)) (transformE b))
    EmptyList -> Null
  where arrayAccessCreator [b] dim = App (Lambda (Var ("~" `append` nthDim dim)) (arrLambda (dim - 1) ("~" `append` nthDim dim) "~lookupArr" Null)) (transformE b)
        arrayAccessCreator (b:bs) dim = App (arrayAccessCreator bs (dim + 1)) (transformE b)
        appHelper func [x] = case func of
          "pair" -> App (BuiltIn func) (transformE x)
          "append" -> App (BuiltIn func) (transformE x)
          "length" -> App (BuiltIn func) (transformE x)
          "head" -> App (BuiltIn func) (transformE x)
          "tail" -> App (BuiltIn func) (transformE x)
          "print" -> App (BuiltIn func) (transformE x)
          "input" -> App (BuiltIn func) (transformE x)
          otherwise -> App (Var func) (transformE x)
        appHelper func (x:xs) = App (appHelper func xs) (transformE x)


transform :: [Statement] -> Term
transform [] = Null
transform [s] = 
  case s of 
    If cond b1 b2 -> (App (App (App (BuiltIn "if") (transformE cond)) (transform b1)) (transform b2))
    For e1 e2 e3 body -> forLoop e1 e2 e3 body []
    Return e -> (transformE e)
    ExpressionStatement e -> App (Lambda (Var "_") Null) (transformE e)
    VariableAssignment [e] e2 -> (App (Lambda (transformE e) Null) (transformE e2))
    VariableAssignment es e2 -> App (arrayAssignmentCreator es 0) (transformE e2)
    FunctionAssignment name vars statements -> App (Lambda (transformE name) Null) (expandFunc vars statements)
  where arrayAssignmentCreator [e] dim = App (Lambda (Var ("~" `append` nthDim dim)) (arrLambda (dim - 1) ("~" `append` nthDim dim) "~setArr" Null)) (transformE e)
        arrayAssignmentCreator (e:es) dim = App (arrayAssignmentCreator es (dim + 1)) (transformE e)
        expandFunc [] statements = transform statements
        expandFunc (v:vars) statements = Lambda (transformE v) (expandFunc vars statements)
transform (s:ss) =
  case s of
    If cond b1 b2 -> (App (App (App (BuiltIn "if") (transformE cond)) (transform (b1++ss))) (transform (b2++ss)))
    For e1 e2 e3 body -> forLoop e1 e2 e3 body ss
    Return e -> (transformE e)
    ExpressionStatement e -> App (Lambda (Var "_") (transform ss)) (transformE e)
    VariableAssignment [e] e2 -> (App (Lambda (transformE e) (transform ss)) (transformE e2))
    VariableAssignment es e2 -> App (arrayAssignmentCreator es 0) (transformE e2)
    FunctionAssignment name vars statements -> App (Lambda (transformE name) (transform ss)) (expandFunc vars statements)
  where arrayAssignmentCreator [e] dim = App (Lambda (Var ("~" `append` nthDim dim)) (arrLambda (dim - 1) ("~" `append` nthDim dim) "~setArr" (transform ss))) (transformE e)
        arrayAssignmentCreator (e:es) dim = App (arrayAssignmentCreator es (dim + 1)) (transformE e)
        expandFunc [] statements = transform statements
        expandFunc (v:vars) statements = Lambda (transformE v) (expandFunc vars statements)

mkStringList [] = Null
mkStringList (s:ss) = App (App (BuiltIn "pair") (Value (CChar s))) (mkStringList ss)