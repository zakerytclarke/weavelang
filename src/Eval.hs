{-# LANGUAGE OverloadedStrings #-}
module Eval (eval) where

import Data.Text (Text,pack,unpack, append)

import Transform
import NewParser

import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.IO.Class (liftIO)

evalS :: StatementTrans -> (StateT [(Text, StatementTrans)] IO) StatementTrans
evalS NullT = return NullT
evalS (VarT name) = do
  frame <- get
  return $ getVar name frame
evalS (Lambda (VarT "~for") rest) = do
  frame <- get
  case getVar "~forBody" frame of
    NullT -> evalS rest
    b -> do
      forVar <- return $ getForVar frame
      put (removeForVar frame)
      (forLoop (forVar++[b]) rest)
evalS (Lambda (VarT "~lookupArr") NullT) = do
  frame <- get
  case getVar "~Arr" frame of
    a@(VarT var) -> do
      eA <- evalS a
      arrVar <- return $ getArrVar frame
      put (removeArrVar arrVar (removeVar "~Arr" frame))
      return $ lookupVarArr arrVar eA
    _ -> error "Error"
evalS (AppT (Lambda (VarT "~setArr") rest) arg) = do
  eArg <- evalS arg
  frame <- get
  case getVar "~Arr" frame of
    a@(VarT var) -> do
      eA <- return $ getVarSafe var frame
      arrVar <- return $ getArrVar frame
      put (setVar var (setArrVar arrVar eA eArg) frame)
      frame <- get
      put (removeArrVar arrVar (removeVar "~Arr" frame))
      evalS rest
    _ -> error "Error"
evalS (AppT (Lambda (VarT name@("~forBody")) sexpr) arg) = do
  frame <- get
  case getVarSafe name frame of
    NullT -> do
      modify (setVar name arg)
      evalS sexpr
    var -> do
      modify (setVar name arg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS (AppT (Lambda (VarT name@("~forVar")) sexpr) arg) = do
  frame <- get
  case getVarSafe name frame of
    NullT -> do
      modify (setVar name arg)
      evalS sexpr
    var -> do
      modify (setVar name arg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS (AppT (Lambda (VarT name@("~Arr")) sexpr) arg) = do
  frame <- get
  case getVarSafe name frame of
    NullT -> do
      modify (setVar name arg)
      evalS sexpr
    var -> do
      modify (setVar name arg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS (AppT (BuiltInT "print") arg) = do
  eArg <- evalS arg
  liftIO $ putStrLn $ prettyPrintStatementTrans eArg
  return eArg
evalS (AppT (BuiltInT "input") arg) = do
  eArg <- evalS arg
  liftIO $ putStrLn $ prettyPrintStatementTrans eArg
  res <- liftIO $ getLine
  return $ mkStringList res
evalS (AppT (AppT (AppT (BuiltInT "if") cond) x) y) = do
  (ConstT (CBool c')) <- evalS cond
  if c'
    then evalS x
    else evalS y
evalS a@(AppT (AppT (BuiltInT op) NullT) NullT) = 
  case op of 
    "==" -> return $ ConstT (CBool True)
    "!=" -> return $ ConstT (CBool False)
evalS a@(AppT (AppT (BuiltInT op) (ConstT x)) (ConstT y)) = 
  constHelper op x y
evalS a@(AppT (AppT (BuiltInT op) var@(VarT name)) NullT) = do
  eVar <- evalS var
  case op of 
    "==" -> return $ ConstT (CBool (eVar == NullT))
    "!=" -> return $ ConstT (CBool (eVar /= NullT))
evalS a@(AppT (AppT (BuiltInT op) NullT) var@(VarT name)) = do
  eVar <- evalS var
  case op of 
    "==" -> return $ ConstT (CBool (eVar == NullT))
    "!=" -> return $ ConstT (CBool (eVar /= NullT))
-- Look if need to evaluate either x or y
evalS a@(AppT (AppT (BuiltInT "pair") x) y@(VarT _)) = do
  eY <- evalS y
  return (AppT (AppT (BuiltInT "pair") x) eY)
evalS a@(AppT (AppT (BuiltInT "pair") (AppT (AppT (BuiltInT "pair") x) y)) z) = return a 
evalS (AppT (BuiltInT "length") x) = do
  eX <- evalS x
  return (ConstT (CInt (unwrapLs eX)))
  where unwrapLs NullT = 0
        unwrapLs (AppT (AppT (BuiltInT "pair") x) y) = (unwrapLs y) + 1
evalS a@(AppT (AppT (BuiltInT "pair") x) y) = walkLs a
  where walkLs (AppT (AppT (BuiltInT "pair") x) y@(VarT _)) = do
          eY <- evalS y
          return $ (AppT (AppT (BuiltInT "pair") x) eY)
        walkLs (AppT (AppT (BuiltInT "pair") x@(VarT _)) y) = do
          eX <- evalS x
          rest <- walkLs y
          return $ (AppT (AppT (BuiltInT "pair") eX) rest)
        walkLs NullT = return NullT
        walkLs (AppT (AppT (BuiltInT "pair") x) y) = do
          rest <- walkLs y
          return $ (AppT (AppT (BuiltInT "pair") x) rest)
evalS (AppT (BuiltInT "head") x) = do
  eX <- evalS x
  unwrapLs eX
  where unwrapLs NullT = error ("Head applied to empty list")
        unwrapLs (AppT (AppT (BuiltInT "pair") x) y) = evalS x
evalS (AppT (BuiltInT "tail") x) = do
  eX <- evalS x
  unwrapLs eX
  where unwrapLs NullT = error ("Head applied to empty list")
        unwrapLs (AppT (AppT (BuiltInT "pair") x) y) = evalS y
evalS (AppT (AppT (BuiltInT "append") x) y) = do
  eX <- evalS x
  evalS $ unwrapLs eX
  where unwrapLs NullT = y
        unwrapLs (AppT (AppT (BuiltInT "pair") x) y) = (AppT (AppT (BuiltInT "pair") x) (unwrapLs y))
evalS a@(AppT (AppT (BuiltInT op) x) y) = do
  eX <- evalS x
  eY <- evalS y
  evalS (AppT (AppT (BuiltInT op) eX) eY)
evalS a@(AppT (AppT (VarT op) x) y) = do
  eX <- evalS x
  eY <- evalS y
  eOp <- evalS (VarT op)
  evalS (AppT (AppT eOp eX) eY)
evalS (AppT (Lambda (VarT name) sexpr) arg) = do
  eArg <- evalS arg
  frame <- get
  case getVarSafe name frame of
    NullT -> do
      modify (setVar name eArg)
      evalS sexpr
    var -> do
      modify (setVar name eArg)
      es <- evalS sexpr
      modify (setVar name var)
      return es
evalS a@(AppT sexpr arg) = do
  eSexpr <- evalS sexpr
  evalS (AppT eSexpr arg)
evalS x = do
  return x

eval x = runStateT (evalS x) []

nthDim :: Integer -> Text
nthDim 1 = "1st"
nthDim 2 = "2nd"
nthDim 3 = "3rd"
nthDim n = (pack $ show n) `append` "nth"

setArrVar [((ConstT (CInt var)), name)] NullT val = createList var NullT val
  where createList 0 end val = (AppT (AppT (BuiltInT "pair") val) end)
        createList n end val = (AppT (AppT (BuiltInT "pair") NullT) (createList (n-1) end val))
setArrVar [((ConstT (CInt var)), name)] a@(AppT (AppT (BuiltInT "pair") x) y) val = walkDim var a
  where walkDim 0 (AppT (AppT (BuiltInT "pair") x) y) = (AppT (AppT (BuiltInT "pair") val) y)
        walkDim 0 NullT = (AppT (AppT (BuiltInT "pair") val) NullT)
        walkDim n (AppT (AppT (BuiltInT "pair") x) y) = (AppT (AppT (BuiltInT "pair") x) (walkDim (n - 1) y))
        walkDim n NullT = (AppT (AppT (BuiltInT "pair") NullT) (walkDim (n - 1) NullT))
setArrVar (((ConstT (CInt var)), name):xs) NullT val = createNestedList var (setArrVar xs NullT val)
  where createNestedList 0 right = (AppT (AppT (BuiltInT "pair") right) NullT)
        createNestedList n right = (AppT (AppT (BuiltInT "pair") NullT) (createNestedList (n - 1) right))
setArrVar (((ConstT (CInt var)), name):xs) a@(AppT (AppT (BuiltInT "pair") x) y) val = walkDim var a
  where walkDim 0 NullT = (AppT (AppT (BuiltInT "pair") (setArrVar xs NullT val)) NullT)
        walkDim 0 (AppT (AppT (BuiltInT "pair") x) y) = AppT (AppT (BuiltInT "pair") (setArrVar xs x val)) y
        walkDim n (AppT (AppT (BuiltInT "pair") x) y) = (AppT (AppT (BuiltInT "pair") x) (walkDim (n - 1) y))
        walkDim n NullT = (AppT (AppT (BuiltInT "pair") NullT) (walkDim (n - 1) NullT))

lookupVarArr :: [(StatementTrans,Text)] -> StatementTrans -> StatementTrans
lookupVarArr [((ConstT (CInt var)), name)] NullT = error "Index out of range"
lookupVarArr  [((ConstT (CInt var)), name)] a@(AppT (AppT (BuiltInT "pair") x) y) = walkDim var a
  where walkDim 0 (AppT (AppT (BuiltInT "pair") x ) y) = x
        walkDim 0 NullT = error "Index out of range"
        walkDim n (AppT (AppT (BuiltInT "pair") x ) y) = walkDim (n - 1) y
        walkDim n NullT = error "Index out of range"
lookupVarArr (((ConstT (CInt var)), name):xs) NullT = error "Index out of range"
lookupVarArr (((ConstT (CInt var)), name):xs) a@(AppT (AppT (BuiltInT "pair") x) y) = walkDim var a
  where walkDim 0 (AppT (AppT (BuiltInT "pair") x) y) = lookupVarArr xs x
        walkDim 0 NullT = error "Index out of range"
        walkDim n (AppT (AppT (BuiltInT "pair") x) y) = walkDim (n - 1) y
        walkDim n NullT = error "Index out of range"

--- GET ALL ARR VARIABLES FROM FRAME "~1st" ...
getArrVar frame = getArrVarHelper frame 1
  where getArrVarHelper frame count = 
          case getVarSafe ("~" `append` (nthDim count)) frame of
            NullT -> []
            var -> (var, ("~" `append` (nthDim count)) ) : getArrVarHelper frame (count + 1)

removeArrVar :: [(StatementTrans, Text)] -> [(Text, StatementTrans)] -> [(Text, StatementTrans)]
removeArrVar [] frame = frame
removeArrVar ((_, name):xs) frame = removeArrVar xs (removeVar name frame)

forLoop :: [StatementTrans] -> StatementTrans -> (StateT [(Text, StatementTrans)] IO) StatementTrans
forLoop [start,end,var,body] rest = do
  eStart <- evalS start
  case eStart of
    (ConstT (CInt startInt)) -> do
      eEnd <- evalS end
      case eEnd of 
        (ConstT (CInt endInt)) -> do
          if (startInt < endInt) 
            then evalS $ forLoopHelper startInt endInt var body rest
            else evalS rest
        _ -> error "For loop end index not an int"
    _ -> error "For loop start index not an int"
forLoop xs rest = error "For loop error"

forLoopHelper current e var b rest =
  if current > e
    then rest
    else (AppT (Lambda (VarT "_") (forLoopHelper (current + 1) e var b rest)) (AppT (Lambda var b) (ConstT (CInt current))))

getForVar frame = getForVarHelper ["~forStart", "~forEnd", "~forVar"] frame
  where getForVarHelper [] frame = []
        getForVarHelper (f:fs) frame = (getVar f frame) : getForVarHelper fs frame

removeForVar frame = removeForVarHelper ["~forStart", "~forEnd", "~forVar", "~forBody"] frame
  where removeForVarHelper [] frame = frame
        removeForVarHelper (f:fs) frame = removeForVarHelper fs (removeVar f frame)

getVarSafe name [] = NullT
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
    then return $ (ConstT (CBool True))
    else return $ (ConstT (CBool False))


-- TODO add all the types and values
constHelper op (CInt x) (CInt y) = 
  case op of 
    "<=" -> comparisonHelper (<=) x y
    "==" -> comparisonHelper (==) x y
    "*" -> return (ConstT (CInt (x * y)))
    "-" -> return (ConstT (CInt (x - y)))
    "+" -> return (ConstT (CInt (x + y)))
    "%" -> return (ConstT (CInt (x `mod` y)))
    "/" -> return (ConstT (CInt (x `div` y)))
constHelper op (CChar x) (CChar y) = 
  case op of 
    "<=" -> comparisonHelper (<=) x y
    "==" -> comparisonHelper (==) x y

b = do
  a <- parseTestFile
  return (transform a)

mkStringList [] = NullT
mkStringList (s:ss) = AppT (AppT (BuiltInT "pair") (ConstT (CChar s))) (mkStringList ss)