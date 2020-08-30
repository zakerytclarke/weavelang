module Runtime (eval) where
import Parser
import Analyzer
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

evalS :: EvalTerm -> (StateT [[(String, EvalTerm)]] IO) EvalTerm
evalS (I "null") = return $ (B (-1))
evalS (I name) = do
  frame <- get
  return $ getVar name frame

--Synchronize Printing  
evalS (A (L "_" sexpr) (A (I "print") arg)) = do
  eArg <- evalS arg
  liftIO (putStrLn $ show eArg)
  evalS sexpr

--Normal Evaluation B Reduction
evalS (A (L name sexpr) arg) = do
  frame <- get
  eArg <- (evalS arg)
  -- Create new frame every application
  put ((setVar name eArg []):frame)
  eSexpr <- evalS sexpr
  modify tail
  return eSexpr
  

evalS (A (A (A (I "~if") cond) x) y) = do
  evalS (AB (AB (AB (I "if") cond) x) y)
--Built in functions
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
  "==" -> comparisionHelper (==) x y
  "!=" -> comparisionHelper (/=) x y
evalS (AB (AB (I op) (C x)) (C y)) = case op of 
  "==" -> comparisionHelper (==) x y
  "!=" -> comparisionHelper (/=) x y
  "<=" -> comparisionHelper (<=) x y
  ">=" -> comparisionHelper (>=) x y
  "<" -> comparisionHelper (<) x y
  ">" -> comparisionHelper (>) x y
evalS (AB (AB (I op) (N x)) (N y)) = case op of 
  "+" -> return $ N (x+y)
  "-" -> return $ N (x-y)
  "*" -> return $ N (x*y)
  "^" -> return $ N (x**y)
  "/" -> return $ N (x/y)
  "==" -> comparisionHelper (==) x y
  "!=" -> comparisionHelper (/=) x y
  "<=" -> comparisionHelper (<=) x y
  ">=" -> comparisionHelper (>=) x y
  "<" -> comparisionHelper (<) x y
  ">" -> comparisionHelper (>) x y
evalS (AB (AB (AB (I op) (B x)) y) z) = case op of
  "if" -> do
      if (x == 1) 
        then (evalS y) 
        else (evalS z)

evalS (AB (AB (AB op x) y) z) = do 
  a <- evalS x
  evalS (AB (AB (AB op a) y) z)



--Recursive Evaluations

evalS (AB x y) = do
  a <- evalS x
  b <- evalS y
  evalS (AB a b)


evalS x = return x

-- Run the state of the eval
eval x = runStateT (evalS x) []


-- Comparision Helper
comparisionHelper func x y = 
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

setVar name val [] = [(name,val)]
setVar name val (x:xs) = 
  if ((fst x)==name)
    then (name,val):xs
    else x:(setVar name val xs)
