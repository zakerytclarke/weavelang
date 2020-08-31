module Runtime (eval) where
import Parser
import Analyzer
import Data.Char
import Data.List
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)


evalS :: EvalTerm -> (StateT [[(String, EvalTerm)]] IO) EvalTerm
evalS (I "[]") = return $ (B (-1))
evalS (I "null") = return $ (B (-1))
-- All functions should only look in their closure
evalS (I name) = do
  frame <- get
  return $ getVar name frame

        
--Synchronize Printing  
evalS (A (L "_" sexpr) (A (I "print") arg)) = do
  eArg <- evalS arg
  liftIO (putStrLn $ show eArg)
  evalS sexpr
--Synchronize Input
evalS (A (L name sexpr) (A (I "input") arg)) = do
  eArg <- evalS arg
  liftIO (putStrLn $ show eArg)
  uIn <- liftIO $ getLine
  if (name=="_")
    then evalS sexpr
    else do
      evalS (A (L name sexpr) (inputMapper uIn))
  where inputMapper [] = (B (-1))
        inputMapper (x:xs) = (AB (AB (I "pair") (C (fromIntegral $ (ord x)))) (inputMapper xs)) 

--Assign to Array

evalS (A (L "~assignArr" rest) (A (I name) (A index term))) = do
  eIndex <- evalS index
  eTerm <- evalS term
  (frame:rs) <- get
  let ls = getVarSafe name (frame:rs)
  case eIndex of
    (N i) -> do
              put ((setVar name (unwrapLs ls i eTerm) frame):rs)
              a <- get
              evalS rest
  where unwrapLs (B (-1)) n eTerm = unwrapLs (I "[]") n eTerm
        unwrapLs (I "null") n eTerm = unwrapLs (I "[]") n eTerm
        unwrapLs (I "[]") 0 eTerm = (AB (AB (I "pair") eTerm) (I "[]")) --Poorly formed lists
        unwrapLs (I "[]") n eTerm = (AB (AB (I "pair") (I "[]")) (unwrapLs (I "[]") (n-1) eTerm))
        unwrapLs (A (A (I "pair") x) xs) 0 eTerm = (AB (AB (I "pair") eTerm) xs)
        unwrapLs (AB (AB (I "pair") x) xs) 0 eTerm = (AB (AB (I "pair") eTerm) xs)
        unwrapLs (A (A (I "pair") x) xs) n eTerm =  (AB (AB (I "pair") x) (unwrapLs xs (n-1) eTerm)) 
        unwrapLs (AB (AB (I "pair") x) xs) n eTerm = (AB (AB (I "pair") x) (unwrapLs xs (n-1) eTerm)) 
        unwrapLs a n eTerm = error (show a)


--List functions
evalS (A (A (I "~lookupArr") (I name)) (N index)) = do
  frame <- get
  unwrapLs (getVar name frame) index
  where unwrapLs (I "[]") n = error ("Element "++(show index)++" not defined for list "++(show name))
        unwrapLs (A (A (I "pair") x) xs) 0 = evalS x
        unwrapLs (A (A (I "pair") x) xs) n = unwrapLs xs (n - 1)
        unwrapLs (AB (AB (I "pair") x) xs) 0 = evalS x
        unwrapLs (AB (AB (I "pair") x) xs) n = unwrapLs xs (n - 1)

evalS (A (A (I "~lookupArr") (I name)) index) = do
  eIndex <- evalS index
  evalS (A (A (I "~lookupArr") (I name)) eIndex)

evalS (A (A (I "append") x) y) = do
  eX <- evalS x
  eY <- evalS y
  return $ unwrapLs eX eY
  where unwrapLs (B (-1.0)) eT = eT
        unwrapLs (A (A (I "pair") x) xs) eT = (A (A (I "pair") x) (unwrapLs xs eT))
        unwrapLs (AB (AB (I "pair") x) xs) eT = (AB (AB (I "pair") x) (unwrapLs xs eT))
  

evalS (A (I "head") (I name)) = evalS (A (A (I "~lookupArr") (I name)) (N 0)) 
evalS (A (I "head") arg) = do
  eArg <- evalS arg
  unwrapLs eArg
  where unwrapLs (I "[]") = error ("Head applied to empty list")
        unwrapLs (A (A (I "pair") x) xs) = evalS x
        unwrapLs (AB (AB (I "pair") x) xs) = evalS x

evalS (A (I "tail") (I name)) = do
  frame <- get
  unwrapLs (getVar name frame)
  where unwrapLs (I "[]") = error ("Tail applied to empty list: "++(show name))
        unwrapLs (A (A (I "pair") x) xs) = evalS xs
        unwrapLs (AB (AB (I "pair") x) xs) = evalS xs
evalS (A (I "tail") arg) = do
  eArg <- evalS arg
  unwrapLs eArg
  where unwrapLs (I "[]") = error ("Tail applied to empty list")
        unwrapLs (A (A (I "pair") x) xs) = evalS xs
        unwrapLs (AB (AB (I "pair") x) xs) = evalS xs

evalS (A (I "length") arg) = do
  eArg <- evalS arg
  return (N (unwrapLs eArg))
  where unwrapLs (B (-1.0)) = 0
        unwrapLs (A (A (I "pair") x) xs) = (unwrapLs xs)+1
        unwrapLs (AB (AB (I "pair") x) xs) = (unwrapLs xs)+1

--Normal Evaluation B Reduction
evalS (A (L name sexpr) arg) = do
  eArg <- (evalS arg)
  -- Create new frame every application
  -- TODO FIGURE OUT WHERE TO HANDLE FRAMES
  -- Change this to closures instead of frames
  -- DOES THIS HANDLE EVERY CASE???
  modify (setVar name eArg [] :)
  eSexpr <- evalS sexpr
  case eSexpr of
    a@(L _ _) -> return a 
    a@_ -> do
      modify tail
      return a

evalS (A (A (A (I "~if") cond) x) y) = do
  evalS (AB (AB (AB (I "if") cond) x) y)
--Built in functions
evalS (A (I ("~!")) arg) = do 
  eArg <- evalS arg
  evalS (AB (I ("!")) eArg)
evalS (A (I ('~':name)) arg) = do 
  eArg <- evalS arg
  return $ (AB (I (name)) eArg)
--Stop recursion built-in hit  
evalS (A ab@(AB _ _) arg) = do
  eArg <- evalS arg
  evalS (AB ab eArg)
--Application  
evalS (A sexpr arg) = do
  eSexpr <- evalS sexpr
  evalS (A eSexpr arg)

--Base Case Built in functions
--Mathematical Reduction to Final Solution
evalS (AB (AB (I op) (B x)) (B y)) = case op of 
  "&&" -> 
    if x == y && x == 1
      then return $ (B 1)
      else return $ (B 0)
  "||" -> 
    if x == 1 || y == 1
      then return $ (B 1)
      else return $ (B 0)
  "==" -> comparisonHelper (==) x y
  "!=" -> comparisonHelper (/=) x y
evalS (AB (AB (I op) (C x)) (C y)) = case op of 
  "==" -> comparisonHelper (==) x y
  "!=" -> comparisonHelper (/=) x y
  "<=" -> comparisonHelper (<=) x y
  ">=" -> comparisonHelper (>=) x y
  "<" -> comparisonHelper (<) x y
  ">" -> comparisonHelper (>) x y
evalS (AB (AB (I op) (N x)) (N y)) = case op of 
  "+" -> return $ N (x+y)
  "-" -> return $ N (x-y)
  "*" -> return $ N (x*y)
  "^" -> return $ N (x**y)
  "/" -> return $ N (x/y)
  "%" -> return $ N (fromIntegral $ (round x) `rem` (round y))
  "==" -> comparisonHelper (==) x y
  "!=" -> comparisonHelper (/=) x y
  "<=" -> comparisonHelper (<=) x y
  ">=" -> comparisonHelper (>=) x y
  "<" -> comparisonHelper (<) x y
  ">" -> comparisonHelper (>) x y
evalS (AB (AB (AB (I op) (B x)) y) z) = case op of
  "if" -> do
      if (x == 1) 
        then (evalS y) 
        else (evalS z)
--Lists
evalS (AB (AB (I "pair") x) rest) = return $ (AB (AB (I "pair") x) rest)

evalS (AB (AB (AB op x) y) z) = do 
  a <- evalS x
  evalS (AB (AB (AB op a) y) z)
evalS (AB (I "!") (B x)) = 
  if x == 1
    then return $ (B 0)
    else return $ (B 1)
evalS (AB (I "!") x) = do
  a <- evalS x
  evalS (AB (I "!") a)
--Recursive Evaluations

evalS (AB x y) = do
  a <- evalS x
  b <- evalS y
  evalS (AB a b)


evalS x = return x

-- Run the state of the eval
eval x = runStateT (evalS x) [[]]


-- Comparision Helper
comparisonHelper func x y = 
  if x `func` y
    then return $ (B 1)
    else return $ (B 0)



--Variable Type Frame Helpers
getVar name [] = error ("Undefined variable: "++ show name)
getVar name (f:fs) = getVarHelper name f fs
  where getVarHelper name [] [] = error ("Undefined variable: "++ show name)
        getVarHelper name [] (r:rest) = getVarHelper name r rest
        getVarHelper name (f:fs) rest = 
          if (fst f) == name 
            then snd f 
            else getVarHelper name fs rest


getVarSafe name [] = (I "null")
getVarSafe name (f:fs) = getVarHelper name f fs
  where getVarHelper name [] [] = (I "null")
        getVarHelper name [] (r:rest) = getVarHelper name r rest
        getVarHelper name (f:fs) rest = 
          if (fst f) == name 
            then snd f 
            else getVarHelper name fs rest

setVar name val [] = [(name,val)]
setVar name val (x:xs) = 
  if ((fst x)==name)
    then (name,val) : xs
    else x:(setVar name val xs)



