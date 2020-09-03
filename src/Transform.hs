{-# LANGUAGE OverloadedStrings #-}

module Transform (StatementTrans(..), transform, prettyPrintStatementTrans) where

import NewParser
import Data.Text (Text,pack,unpack, append)

data StatementTrans = AppT StatementTrans StatementTrans
  | Lambda StatementTrans StatementTrans
  | VarT Text
  | BuiltInT Text
  | NullT
  | ConstT Const
  deriving (Show,Eq)

getVar name [] = error ("Undefined variable: "++ show name)
getVar name (f:fs) = 
  if (fst f)==name
    then (snd f)
    else getVar name fs

prettyPrintStatementTrans :: StatementTrans -> String
prettyPrintStatementTrans (ConstT a) = show a
prettyPrintStatementTrans NullT = ""
prettyPrintStatementTrans (VarT name) = unpack name
prettyPrintStatementTrans (BuiltInT a) = unpack a
prettyPrintStatementTrans (Lambda a sexpr)  = "(l." ++ prettyPrintStatementTrans a ++ " " ++ prettyPrintStatementTrans sexpr ++ ")"
prettyPrintStatementTrans a@(AppT (AppT (BuiltInT "pair") (ConstT (CChar x))) y) = showStringHelper a 
prettyPrintStatementTrans a@(AppT (AppT (BuiltInT "pair") x) y) = "[" ++ showPairHelper a ++ "]"
prettyPrintStatementTrans (AppT sexpr arg) = "(" ++ prettyPrintStatementTrans sexpr ++ " " ++ prettyPrintStatementTrans arg ++ ")"
{-
instance Show StatementTrans where
  show (ConstT a) = show a
  show NullT = ""
  show (VarT a) = (unpack a)
  show (Lambda a sexpr) = "(l." ++ show a ++ " " ++ show sexpr ++ ")"
  show a@(AppT (AppT (VarT "pair") (ConstT (CChar x))) y) = showStringHelper a
  show a@(AppT (AppT (VarT "pair") x) y) = "[" ++ showPairHelper a ++ "]"
  show (AppT sexpr arg) = "(" ++ show sexpr ++ " " ++ show arg ++ ")"
-}
showStringHelper (AppT (AppT (BuiltInT "pair") (ConstT (CChar x))) NullT) = [x]
showStringHelper (AppT (AppT (BuiltInT "pair") (ConstT (CChar x))) y) = [x] ++ showStringHelper y 
showStringHelper (AppT (AppT (BuiltInT "pair") x) y) = prettyPrintStatementTrans x ++ showStringHelper y 

showPairHelper (AppT (AppT (BuiltInT "pair") x) NullT) = prettyPrintStatementTrans x 
showPairHelper (AppT (AppT (BuiltInT "pair") NullT) y) = "  " ++ "," ++ showPairHelper y 
showPairHelper (AppT (AppT (BuiltInT "pair") x) y) = prettyPrintStatementTrans x ++ "," ++ showPairHelper y 

nthDim :: Integer -> Text
nthDim 1 = "1st"
nthDim 2 = "2nd"
nthDim 3 = "3rd"
nthDim n = (pack $ show n) `append` "nth"

arrLambda :: Integer -> Text -> Text -> StatementTrans -> StatementTrans
arrLambda 0 name func end = (Lambda (VarT ("~Arr")) (Lambda (VarT func) end))
arrLambda n name func end = (Lambda (VarT (append "~" (nthDim n))) (arrLambda (n - 1) (append (append "~" (nthDim n)) name) func end))

forLoop var start end body rest = 
  (AppT 
    (AppT 
      (AppT 
        (AppT
          (Lambda (VarT "~forStart") 
            (Lambda (VarT "~forEnd") 
              (Lambda (VarT "~forVar") 
                (Lambda (VarT "~forBody") 
                  (Lambda (VarT "~for") (transform rest))
                )
              )
            )
          ) (transformE start) 
        ) (transformE end)
      ) (transformE var)
    ) (transform body)
  )

transformE :: Expression -> StatementTrans
transformE e =
  case e of
    Const (CString str) -> mkStringList str
    Const c -> ConstT c 
    Var c -> case c of
      "pair" -> BuiltInT c
      "append" -> BuiltInT c
      "length" -> BuiltInT c
      "head" -> BuiltInT c
      "tail" -> BuiltInT c
      "print" -> BuiltInT c
      "input" -> BuiltInT c
      otherwise -> VarT c
    ArrayAccess a bs -> (arrayAccessCreator (a:bs) 0)
    BinaryOp ":" a b -> AppT (AppT (BuiltInT "pair") (transformE a)) (transformE b)
    BinaryOp "++" a b -> AppT (AppT (BuiltInT "append") (transformE a)) (transformE b)
    BinaryOp op a b -> AppT (AppT (BuiltInT op) (transformE a)) (transformE b)
    Not e -> AppT (BuiltInT "!") (transformE e)
    Negate e -> AppT (BuiltInT "-") (transformE e)
    App (Var c) args -> appHelper c (reverse args)
    Pair a b -> (AppT (AppT (BuiltInT "pair") (transformE a)) (transformE b))
    EmptyList -> NullT
  where arrayAccessCreator [b] dim = AppT (Lambda (VarT ("~" `append` nthDim dim)) (arrLambda (dim - 1) ("~" `append` nthDim dim) "~lookupArr" NullT)) (transformE b)
        arrayAccessCreator (b:bs) dim = AppT (arrayAccessCreator bs (dim + 1)) (transformE b)
        appHelper func [x] = case func of
          "pair" -> AppT (BuiltInT func) (transformE x)
          "append" -> AppT (BuiltInT func) (transformE x)
          "length" -> AppT (BuiltInT func) (transformE x)
          "head" -> AppT (BuiltInT func) (transformE x)
          "tail" -> AppT (BuiltInT func) (transformE x)
          "print" -> AppT (BuiltInT func) (transformE x)
          "input" -> AppT (BuiltInT func) (transformE x)
          otherwise -> AppT (VarT func) (transformE x)
        appHelper func (x:xs) = AppT (appHelper func xs) (transformE x)


transform :: [Statement] -> StatementTrans
transform [] = NullT
transform [s] = 
  case s of 
    If cond b1 b2 -> (AppT (AppT (AppT (BuiltInT "if") (transformE cond)) (transform b1)) (transform b2))
    For e1 e2 e3 body -> forLoop e1 e2 e3 body []
    Return e -> (transformE e)
    ExpressionStatement e -> AppT (Lambda (VarT "_") NullT) (transformE e)
    VariableAssignment [e] e2 -> (AppT (Lambda (transformE e) NullT) (transformE e2))
    VariableAssignment es e2 -> AppT (arrayAssignmentCreator es 0) (transformE e2)
    FunctionAssignment name vars statements -> AppT (Lambda (transformE name) NullT) (expandFunc vars statements)
  where arrayAssignmentCreator [e] dim = AppT (Lambda (VarT ("~" `append` nthDim dim)) (arrLambda (dim - 1) ("~" `append` nthDim dim) "~setArr" NullT)) (transformE e)
        arrayAssignmentCreator (e:es) dim = AppT (arrayAssignmentCreator es (dim + 1)) (transformE e)
        expandFunc [] statements = transform statements
        expandFunc (v:vars) statements = Lambda (transformE v) (expandFunc vars statements)
transform (s:ss) =
  case s of
    If cond b1 b2 -> (AppT (AppT (AppT (BuiltInT "if") (transformE cond)) (transform (b1++ss))) (transform (b2++ss)))
    For e1 e2 e3 body -> forLoop e1 e2 e3 body ss
    Return e -> (transformE e)
    ExpressionStatement e -> AppT (Lambda (VarT "_") (transform ss)) (transformE e)
    VariableAssignment [e] e2 -> (AppT (Lambda (transformE e) (transform ss)) (transformE e2))
    VariableAssignment es e2 -> AppT (arrayAssignmentCreator es 0) (transformE e2)
    FunctionAssignment name vars statements -> AppT (Lambda (transformE name) (transform ss)) (expandFunc vars statements)
  where arrayAssignmentCreator [e] dim = AppT (Lambda (VarT ("~" `append` nthDim dim)) (arrLambda (dim - 1) ("~" `append` nthDim dim) "~setArr" (transform ss))) (transformE e)
        arrayAssignmentCreator (e:es) dim = AppT (arrayAssignmentCreator es (dim + 1)) (transformE e)
        expandFunc [] statements = transform statements
        expandFunc (v:vars) statements = Lambda (transformE v) (expandFunc vars statements)

mkStringList [] = NullT
mkStringList (s:ss) = AppT (AppT (BuiltInT "pair") (ConstT (CChar s))) (mkStringList ss)