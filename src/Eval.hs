{-# LANGUAGE OverloadedStrings #-}
module Eval (eval) where

import Data.Text (Text,pack,unpack, append)

import Transform
import Parser

import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.IO.Class (liftIO)

evalS :: Term -> (StateT [(Text, Term)] IO) Term
evalS Null = return Null
evalS (Var name) = do
  frame <- get
  return $ getVar name frame
evalS (Lambda (Var "~for") rest) = do
  frame <- get
  case getVar "~forBody" frame of
    Null -> evalS rest
    b -> do
      forVar <- return $ getForVar frame
      put (removeForVar frame)
      (forLoop (forVar++[b]) rest)
evalS (Lambda (Var "~lookupArr") Null) = do
  frame <- get
  case getVar "~Arr" frame of
    a@(Var var) -> do
      eA <- evalS a
      arrVar <- return $ getArrVar frame
      put (removeArrVar arrVar (removeVar "~Arr" frame))
      return $ lookupVarArr arrVar eA
    _ -> error "Error"
evalS (App (Lambda (Var "~setArr") rest) arg) = do
  eArg <- evalS arg
  frame <- get
  case getVar "~Arr" frame of
    a@(Var var) -> do
      eA <- return $ getVarSafe var frame
      arrVar <- return $ getArrVar frame
      put (setVar var (setArrVar arrVar eA eArg) frame)
      frame <- get
      put (removeArrVar arrVar (removeVar "~Arr" frame))
      evalS rest
    _ -> error "Error"
evalS (App (Lambda (Var name@("~forBody")) sexpr) arg) = do
  frame <- get
  case getVarSafe name frame of
    Null -> do
      modify (setVar name arg)
      evalS sexpr
    var -> do
      modify (setVar name arg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS (App (Lambda (Var name@("~forVar")) sexpr) arg) = do
  frame <- get
  case getVarSafe name frame of
    Null -> do
      modify (setVar name arg)
      evalS sexpr
    var -> do
      modify (setVar name arg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS (App (Lambda (Var name@("~Arr")) sexpr) arg) = do
  frame <- get
  case getVarSafe name frame of
    Null -> do
      modify (setVar name arg)
      evalS sexpr
    var -> do
      modify (setVar name arg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS (App (BuiltIn "print") arg) = do
  eArg <- evalS arg
  liftIO $ putStrLn $ prettyPrintStatementTrans eArg
  return eArg
evalS (App (BuiltIn "input") arg) = do
  eArg <- evalS arg
  liftIO $ putStrLn $ prettyPrintStatementTrans eArg
  res <- liftIO $ getLine
  return $ mkStringList res
evalS (App (App (App (BuiltIn "if") cond) x) y) = do
  (Value (CBool c')) <- evalS cond
  if c'
    then evalS x
    else evalS y
evalS a@(App (App (BuiltIn op) Null) Null) = 
  case op of 
    "==" -> return $ Value (CBool True)
    "!=" -> return $ Value (CBool False)
evalS a@(App (App (BuiltIn op) (Value x)) (Value y)) = 
  constHelper op x y
evalS a@(App (App (BuiltIn op) var@(Var name)) Null) = do
  eVar <- evalS var
  case op of 
    "==" -> return $ Value (CBool (eVar == Null))
    "!=" -> return $ Value (CBool (eVar /= Null))
evalS a@(App (App (BuiltIn op) Null) var@(Var name)) = do
  eVar <- evalS var
  case op of 
    "==" -> return $ Value (CBool (eVar == Null))
    "!=" -> return $ Value (CBool (eVar /= Null))
-- Look if need to evaluate either x or y
evalS a@(App (App (BuiltIn "pair") x) y@(Var _)) = do
  eY <- evalS y
  return (App (App (BuiltIn "pair") x) eY)
evalS a@(App (App (BuiltIn "pair") (App (App (BuiltIn "pair") x) y)) z) = return a 
evalS (App (BuiltIn "length") x) = do
  eX <- evalS x
  return (Value (CInt (unwrapLs eX)))
  where unwrapLs Null = 0
        unwrapLs (App (App (BuiltIn "pair") x) y) = (unwrapLs y) + 1
evalS a@(App (App (BuiltIn "pair") x) y) = do
  eX <- evalS x
  eY <- evalS y
  walkLs (App (App (BuiltIn "pair") eX) eY)
  where walkLs (App (App (BuiltIn "pair") x) y@(Var _)) = do
          eY <- evalS y
          return $ (App (App (BuiltIn "pair") x) eY)
        walkLs (App (App (BuiltIn "pair") x@(Var _)) y) = do
          eX <- evalS x
          rest <- walkLs y
          return $ (App (App (BuiltIn "pair") eX) rest)
        walkLs Null = return Null
        walkLs (App (App (BuiltIn "pair") x) y) = do
          rest <- walkLs y
          return $ (App (App (BuiltIn "pair") x) rest)
evalS (App (BuiltIn "head") x) = do
  eX <- evalS x
  unwrapLs eX
  where unwrapLs Null = error ("Head applied to empty list")
        unwrapLs (App (App (BuiltIn "pair") x) y) = evalS x
evalS (App (BuiltIn "tail") x) = do
  eX <- evalS x
  unwrapLs eX
  where unwrapLs Null = error ("Head applied to empty list")
        unwrapLs (App (App (BuiltIn "pair") x) y) = evalS y
evalS (App (App (BuiltIn "append") x) y) = do
  eX <- evalS x
  evalS $ unwrapLs eX
  where unwrapLs Null = y
        unwrapLs (App (App (BuiltIn "pair") x) y) = (App (App (BuiltIn "pair") x) (unwrapLs y))
evalS a@(App (App (BuiltIn op) x) y) = do
  eX <- evalS x
  eY <- evalS y
  evalS (App (App (BuiltIn op) eX) eY)
evalS a@(App (App (Var op) x) y) = do
  eX <- evalS x
  eY <- evalS y
  eOp <- evalS (Var op)
  evalS (App (App eOp eX) eY)
evalS (App (Lambda (Var name) sexpr) arg) = do
  eArg <- evalS arg
  frame <- get
  case getVarSafe name frame of
    Null -> do
      modify (setVar name eArg)
      evalS sexpr
    var -> do
      modify (setVar name eArg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS a@(App sexpr arg) = do
  eSexpr <- evalS sexpr
  evalS (App eSexpr arg)
evalS x = do
  return x

eval x = runStateT (evalS x) []

nthDim :: Integer -> Text
nthDim 1 = "1st"
nthDim 2 = "2nd"
nthDim 3 = "3rd"
nthDim n = (pack $ show n) `append` "nth"

setArrVar [((Value (CInt var)), name)] Null val = createList var Null val
  where createList 0 end val = (App (App (BuiltIn "pair") val) end)
        createList n end val = (App (App (BuiltIn "pair") Null) (createList (n-1) end val))
setArrVar [((Value (CInt var)), name)] a@(App (App (BuiltIn "pair") x) y) val = walkDim var a
  where walkDim 0 (App (App (BuiltIn "pair") x) y) = (App (App (BuiltIn "pair") val) y)
        walkDim 0 Null = (App (App (BuiltIn "pair") val) Null)
        walkDim n (App (App (BuiltIn "pair") x) y) = (App (App (BuiltIn "pair") x) (walkDim (n - 1) y))
        walkDim n Null = (App (App (BuiltIn "pair") Null) (walkDim (n - 1) Null))
setArrVar (((Value (CInt var)), name):xs) Null val = createNestedList var (setArrVar xs Null val)
  where createNestedList 0 right = (App (App (BuiltIn "pair") right) Null)
        createNestedList n right = (App (App (BuiltIn "pair") Null) (createNestedList (n - 1) right))
setArrVar (((Value (CInt var)), name):xs) a@(App (App (BuiltIn "pair") x) y) val = walkDim var a
  where walkDim 0 Null = (App (App (BuiltIn "pair") (setArrVar xs Null val)) Null)
        walkDim 0 (App (App (BuiltIn "pair") x) y) = App (App (BuiltIn "pair") (setArrVar xs x val)) y
        walkDim n (App (App (BuiltIn "pair") x) y) = (App (App (BuiltIn "pair") x) (walkDim (n - 1) y))
        walkDim n Null = (App (App (BuiltIn "pair") Null) (walkDim (n - 1) Null))

lookupVarArr :: [(Term,Text)] -> Term -> Term
lookupVarArr [((Value (CInt var)), name)] Null = error "Index out of range"
lookupVarArr  [((Value (CInt var)), name)] a@(App (App (BuiltIn "pair") x) y) = walkDim var a
  where walkDim 0 (App (App (BuiltIn "pair") x ) y) = x
        walkDim 0 Null = error "Index out of range"
        walkDim n (App (App (BuiltIn "pair") x ) y) = walkDim (n - 1) y
        walkDim n Null = error "Index out of range"
lookupVarArr (((Value (CInt var)), name):xs) Null = error "Index out of range"
lookupVarArr (((Value (CInt var)), name):xs) a@(App (App (BuiltIn "pair") x) y) = walkDim var a
  where walkDim 0 (App (App (BuiltIn "pair") x) y) = lookupVarArr xs x
        walkDim 0 Null = error "Index out of range"
        walkDim n (App (App (BuiltIn "pair") x) y) = walkDim (n - 1) y
        walkDim n Null = error "Index out of range"

--- GET ALL ARR VARIABLES FROM FRAME "~1st" ...
getArrVar frame = getArrVarHelper frame 1
  where getArrVarHelper frame count = 
          case getVarSafe ("~" `append` (nthDim count)) frame of
            Null -> []
            var -> (var, ("~" `append` (nthDim count)) ) : getArrVarHelper frame (count + 1)

removeArrVar :: [(Term, Text)] -> [(Text, Term)] -> [(Text, Term)]
removeArrVar [] frame = frame
removeArrVar ((_, name):xs) frame = removeArrVar xs (removeVar name frame)

forLoop :: [Term] -> Term -> (StateT [(Text, Term)] IO) Term
forLoop [start,end,var,body] rest = do
  eStart <- evalS start
  case eStart of
    (Value (CInt startInt)) -> do
      eEnd <- evalS end
      case eEnd of 
        (Value (CInt endInt)) -> do
          if (startInt < endInt) 
            then evalS $ forLoopHelper startInt endInt var body rest
            else evalS rest
        _ -> error "For loop end index not an int"
    _ -> error "For loop start index not an int"
forLoop xs rest = error "For loop error"

forLoopHelper current e var b rest =
  if current > e
    then rest
    else (App (Lambda (Var "_") (forLoopHelper (current + 1) e var b rest)) (App (Lambda var b) (Value (CInt current))))

getForVar frame = getForVarHelper ["~forStart", "~forEnd", "~forVar"] frame
  where getForVarHelper [] frame = []
        getForVarHelper (f:fs) frame = (getVar f frame) : getForVarHelper fs frame

removeForVar frame = removeForVarHelper ["~forStart", "~forEnd", "~forVar", "~forBody"] frame
  where removeForVarHelper [] frame = frame
        removeForVarHelper (f:fs) frame = removeForVarHelper fs (removeVar f frame)

getVarSafe name [] = Null
getVarSafe name (f:fs) = 
  if (fst f)==name
    then (snd f)
    else getVarSafe name fs 

getVar name [] = error ("Undefined variable: "++ show name)
getVar name (f:fs) = 
  if (fst f)==name
    then (snd f)
    else getVar name fs

removeVar name [] = []
removeVar name (f:fs) =
  if (fst f) == name
    then fs
    else f : removeVar name fs

setVar name val [] = [(name,val)]
setVar name val (x:xs) = 
  if ((fst x)==name)
    then (name,val) : xs
    else x:(setVar name val xs)


comparisonHelper func x y =
  if x `func` y
    then return $ (Value (CBool True))
    else return $ (Value (CBool False))


-- TODO add all the types and values
constHelper op (CInt x) (CInt y) = 
  case op of 
    "<=" -> comparisonHelper (<=) x y
    "==" -> comparisonHelper (==) x y
    "*" -> return (Value (CInt (x * y)))
    "-" -> return (Value (CInt (x - y)))
    "+" -> return (Value (CInt (x + y)))
    "%" -> return (Value (CInt (x `mod` y)))
    "/" -> return (Value (CInt (x `div` y)))
constHelper op (CChar x) (CChar y) = 
  case op of 
    "<=" -> comparisonHelper (<=) x y
    "==" -> comparisonHelper (==) x y

b = do
  a <- parseTestFile
  return (transform a)

mkStringList [] = Null
mkStringList (s:ss) = App (App (BuiltIn "pair") (Value (CChar s))) (mkStringList ss)