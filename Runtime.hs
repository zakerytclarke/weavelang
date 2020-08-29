
module Runtime (eval) where
import Parser
import Analyzer
eval x = x 
{- 
eval (App (Lambda "print" rest) [str]) frame = 
  do 
    putStrLn $ show str
    (eval rest frame)


eval (App (Lam "input" rest) [str]) frame = 
  do 
    putStrLn $ show str
    putStr "?"
    --in <- getLine
    (eval rest frame)

-}
{-
--Base Types
eval (Number x) frame = x
eval (Char x) frame = x

--eval (Variable name) frame = getVar name frame

--Strict Evaluation
--eval (Lambda name value terms) frame = eval terms (setVar name (eval value frame) frame)

--Built in Functions
eval (Addition t1 t2) _ = (eval t1)+(eval t2)
eval (Subtraction t1 t2) _ = (eval t1)-(eval t2)
eval (Multiplication t1 t2) _  = (eval t1)*(eval t2)
eval (Division t1 t2) _ = (eval t1)/(eval t2)
eval (Exponentiation t1 t2) _  = (eval t1)^(eval t2)

eval (EvalIf t1 t2 t3) _ = if (eval t1) then (eval t2) else (eval t3)

eval (Not t1) _  = if (eval t1) then Fls else Tru
eval (And t1 t2) _  = if (eval t1)&&(eval t2) then Tru else Fls
eval (Or t1 t2) _ = if (eval t1)||(eval t2) then Tru else Fls


eval (Equals t1 t2) _ = if (eval t1)==(eval t2) then Tru else Fls
eval (GreaterThan t1 t2) _ = if (eval t1)>(eval t2) then Tru else Fls
eval (GreaterThanEqual t1 t2) _ = if (eval t1)>=(eval t2) then Tru else Fls
eval (LessThan t1 t2) _ = if (eval t1)<(eval t2) then Tru else Fls
eval (LessThanEqual t1 t2) _ = if (eval t1)<=(eval t2) then Tru else Fls

--Handle all other Base Types
--eval x frame = x
-}



getVar name ((f,val):fs) = 
  if (name==f)
    then val
    else getVar name fs

setVar name val frame = frame++[(name,val)]