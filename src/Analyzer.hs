module Analyzer (transform,typeCheck, transformEval) where
import Parser
import Data.List  
import Control.Monad.Trans.State

{-
 - Transformer
 - This function converts the imperative syntax of the language 
 - into a more concise functional style that will ensure
 - type safety, efficiency, and allow performance gains
 -}


--Finished Transforming
transform (t:[]) =
  case t of 
    --Drop Return and ignore remaining terms
    (Return term) -> (Return (transform [term]))
    --Function Definition
    (FunctionDef [] terms) -> (transform terms)
    (FunctionDef (a:as) terms) -> (Lambda a (transform [(FunctionDef as terms)]))
    --Variable Assignment to Lambda
    --(Assignment name term) -> (Lambda nam (transform [term]) Null)
    (Assignment name term) -> (App (Lambda name Null) [(transform [term])])
    --If Term needs remaining operations tacked on both ends
    (If bTerm tTerms []) -> (EvalIf bTerm (transform (tTerms)) Null)
    --If Else Term needs raamining operations tacked on both ends
    (If bTerm tTerms fTerms) -> (EvalIf bTerm (transform (tTerms)) (transform (fTerms)))
    --Function Applications
    (FunctionCall name args) -> (App (FuncPtr name) (map (\x -> transform [x]) args))
    --Handle all other cases as identity
    (Addition a b) -> (Addition (transform [a]) (transform [b]))
    (Subtraction a b) -> (Subtraction (transform [a]) (transform [b]))
    (Multiplication a b) -> (Multiplication (transform [a]) (transform [b]))
    (Division a b) -> (Division (transform [a]) (transform [b]))
    (Exponentiation a b) -> (Exponentiation (transform [a]) (transform [b]))
    (Not a) -> (Not (transform [a]))
    (And a b) -> (And (transform [a]) (transform [b]))
    (Or a b) -> (Or (transform [a]) (transform [b]))
    (NotEquals a b) -> (NotEquals (transform [a]) (transform [b]))
    (Equals a b) -> (Equals (transform [a]) (transform [b]))
    (GreaterThan a b) -> (GreaterThan (transform [a]) (transform [b]))
    (GreaterThanEqual a b) -> (GreaterThanEqual (transform [a]) (transform [b]))
    (LessThan a b) -> (LessThan (transform [a]) (transform [b]))
    (LessThanEqual a b) -> (LessThanEqual (transform [a]) (transform [b]))
    (Pair a b) -> (Pair (transform [a]) (transform [b]))
    (Concat a b) -> (Concat (transform [a]) (transform [b]))
    (Head a) -> (Head (transform [a]))
    (Tail a) -> (Tail (transform [a]))
    (Map a b) -> (Map (transform [a]) (transform [b]))
    x -> x



--Transform Based on Term
transform (t:ts) =
  case t of 
    --Function Definition
    (FunctionDef [] terms) -> (transform terms)
    (FunctionDef (a:as) terms) -> (Lambda a (transform [(FunctionDef as terms)]))
    --Variable Assignment to Lambda
    (Assignment name term) -> (App (Lambda name (transform ts)) [(transform [term])])
    --(Assignment name term) -> (Lambda name (transform [term]))
    --If Term needs remaining operations tacked on both ends
    (If bTerm tTerms []) -> (EvalIf bTerm (transform (tTerms++ts)) (transform ts))
    --If Else Term needs raamining operations tacked on both ends
    (If bTerm tTerms fTerms) -> (EvalIf bTerm (transform (tTerms++ts)) (transform (fTerms++ts)))
    --Drop Return and ignore remaining terms
    (Return term) -> Return (transform [term])
    --Function Applications
    (FunctionCall name args) -> (App (FuncPtr name) (map (\x -> transform [x]) args))
    --Handle all other cases as identity
    (Addition a b) -> (Addition (transform [a]) (transform [b]))
    (Subtraction a b) -> (Subtraction (transform [a]) (transform [b]))
    (Multiplication a b) -> (Multiplication (transform [a]) (transform [b]))
    (Division a b) -> (Division (transform [a]) (transform [b]))
    (Exponentiation a b) -> (Exponentiation (transform [a]) (transform [b]))
    (Not a) -> (Not (transform [a]))
    (And a b) -> (And (transform [a]) (transform [b]))
    (Or a b) -> (Or (transform [a]) (transform [b]))
    (NotEquals a b) -> (NotEquals (transform [a]) (transform [b]))
    (Equals a b) -> (Equals (transform [a]) (transform [b]))
    (GreaterThan a b) -> (GreaterThan (transform [a]) (transform [b]))
    (GreaterThanEqual a b) -> (GreaterThanEqual (transform [a]) (transform [b]))
    (LessThan a b) -> (LessThan (transform [a]) (transform [b]))
    (LessThanEqual a b) -> (LessThanEqual (transform [a]) (transform [b]))
    (Pair a b) -> (Pair (transform [a]) (transform [b]))
    (Concat a b) -> (Concat (transform [a]) (transform [b]))
    (Head a) -> (Head (transform [a]))
    (Tail a) -> (Tail (transform [a]))
    (Map a b) -> (Map (transform [a]) (transform [b]))
    x -> x



{-
 - Type Checker
 - This function evaluates the type safety of the program
 - and guarantees there will be no run-time errors
 -}
typeCheckState :: Term -> State (String, [(String, Type)]) Type
typeCheckState Null = return IO

typeCheckState Fls = return Boolean
typeCheckState Tru = return Boolean

typeCheckState (Char x) = return Character
typeCheckState (Number x) = return Numeric

typeCheckState (Addition x y) = expectedTypes [x,y] [Numeric,Numeric] "(+)" Numeric
typeCheckState (Subtraction x y) = expectedTypes [x,y] [Numeric,Numeric] "(-)" Numeric
typeCheckState (Multiplication x y) = expectedTypes [x,y] [Numeric,Numeric] "(*)" Numeric
typeCheckState (Division x y) = expectedTypes [x,y] [Numeric,Numeric] "(/)" Numeric
typeCheckState (Exponentiation x y) = expectedTypes [x,y] [Numeric,Numeric] "(^)" Numeric

typeCheckState (Not x) = expectedTypes [x] [Boolean] "(!)" Boolean
typeCheckState (And x y) = expectedTypes [x,y] [Boolean,Boolean] "(&&)" Boolean
typeCheckState (Or x y) = expectedTypes [x,y] [Boolean,Boolean] "(||)" Boolean
typeCheckState (Equals x y) = do
  tY <- typeCheckState y
  expectedTypes [x,y] [tY,tY] "(==)" Boolean 

typeCheckState (NotEquals x y) = do
  tY <- typeCheckState y
  expectedTypes [x,y] [tY,tY] "(!=)" Boolean 
typeCheckState (LessThan x y) = expectedTypes [x,y] [Numeric,Numeric] "(<)" Boolean
typeCheckState (LessThanEqual x y) = expectedTypes [x,y] [Numeric,Numeric] "(<=)" Boolean
typeCheckState (GreaterThan x y) = expectedTypes [x,y] [Numeric,Numeric] "(>)" Boolean
typeCheckState (GreaterThanEqual x y) = expectedTypes [x,y] [Numeric,Numeric] "(>=)" Boolean

typeCheckState (EvalIf x y z) = do 
  tX <- typeCheckState x
  tY <- typeCheckState y
  expectedTypes [x,y,z] [Boolean, tY, tY] "if" tY 

typeCheckState a@(Pair x y) = do 
  fstType <- typeCheckState x
  expectedTypes (tList a) (take (length (tList a)) $ repeat fstType) "[]" (List fstType)
  where tList (Pair a Null) = [a]
        tList (Pair a b) =  a:(tList b)


-- Return does type inference for function if it can
typeCheckState (Return sexpr) = do
  t <- typeCheckState sexpr
  (funcName,_) <- get
  case t of
    Unknown -> error ("Unable to infer the type for function: "++(show funcName))
    t -> do
      (funcName, frame) <- get
      case fTypeUnwrapper (getType funcName frame) of
        Unknown -> do
          put (funcName, (setType funcName (setfTypeUnwrapper (getType funcName frame) t) frame))
          return t
        t1 -> 
          if (t==t1)
            then return t1
            else error ("Return types of function don't match")
  where fTypeUnwrapper (Func x y) = fTypeUnwrapper y
        fTypeUnwrapper y = y 
        setfTypeUnwrapper (Func x y) t = Func x (setfTypeUnwrapper y t)
        setfTypeUnwrapper Unknown t = t
        
    

-- Get function type
typeCheckState (FuncPtr fname) = do
  (_,frame) <- get
  return (getType fname frame)
typeCheckState (Variable varName) = do
  (_, frame) <- get
  return (getType varName frame)


typeCheckState (App (FuncPtr "print") [arg]) = do
  typeCheckState arg 
  return $ List Character
--Function call
typeCheckState (App f@(FuncPtr fName) args) = do 
  fType <- typeCheckState f
  expectedTypes args (fTypeUnwrapper fType) fName (fTypeReturnUnwrapper fType)
  where fTypeUnwrapper (Func x y) = x : fTypeUnwrapper y
        fTypeUnwrapper y = [] 
        fTypeReturnUnwrapper (Func x y) = fTypeReturnUnwrapper y
        fTypeReturnUnwrapper y = y 

--Definition of functions
typeCheckState (App (Lambda funcName sexpr) [f@(Lambda argName sexpr2)]) = do
  (_, frame) <- get
  -- Add new frame, with current function name
  put (funcName, setType funcName Unknown frame)
  typeCheckState f
  typeCheckState sexpr
--Definition of Variables
typeCheckState (App (Lambda varName sexpr) [val]) = do 
  varType <- typeCheckState val
  (funcName,frame) <- get
  put (funcName, setType varName varType frame)
  typeCheckState sexpr


-- Function Arguments
typeCheckState (Lambda varName sexpr) = do
  (funcName,frame) <- get
  put (funcName, setType varName Unknown frame)
  (funcName,frame) <- get
  put (funcName, setType funcName (addArg (UnknownArg varName) (getType funcName frame)) frame)
  t <- typeCheckState sexpr
  (funcName,newFrame) <- get
  if (getType varName newFrame) == Unknown
    then error ("Unused variable: "++(show varName) ++ " in function: " ++ (show funcName))
    else return $ (Func (getType varName newFrame) t)
    where addArg t (Func x y) = (Func x (addArg t y))
          addArg t y = (Func t y) 

--Todo convert to Arr Char type
typeCheckState (Concat x y) = expectedTypes [x,y] [Character,Character] "(++)" Boolean


--typeCheck :: Term -> (String, [(String, Type)]) -> Type
typeCheck ts initialState = runState (typeCheckState ts) initialState

-- Expected types
-- Makes sure the expected types match the actual types. Does type inference for unknown variables 
-- and sometimes function return types
expectedTypes as ts op r = expectedTypesHelper as ts op 1 r
  where 
    expectedTypesHelper [] _ _ _ r = return r
    expectedTypesHelper (a:as) (t:ts) op n r = do
      aT <- typeCheckState a
      case t==aT of
        True -> (expectedTypesHelper as ts op (n+1) r)
        False -> 
          if (aT==Unknown)
            then 
              case a of
                (Variable name) -> do
                  --Type inference for variable 
                  (funcName, frame) <- get
                  put (funcName, setType name t frame)
                  (funcName, frame) <- get
                  put (funcName, setType funcName (updateArgs name t (getType funcName frame)) frame)
                  (expectedTypesHelper as ts op (n+1) r)
                (App (FuncPtr funcName) sexpr) -> do
                  (fName, frame) <- get
                  if (funcName == fName) 
                    then do
                      put (funcName, setType funcName (updateReturn t (getType funcName frame)) frame)
                      (expectedTypesHelper as ts op (n+1) r)
                    else error errMsg 
                _ -> error errMsg
            else error errMsg
          where updateReturn t (Func x y) = Func x (updateReturn t y)
                updateReturn t y = t
                updateArgs name t (Func x y) = 
                  case x of
                    (UnknownArg aName) -> 
                      if (aName == name)
                        then (Func t y)
                        else (Func x (updateArgs name t y))
                    _ -> (Func x (updateArgs name t y))
                updateArgs name t y = y 
                errMsg = ("\nError in " ++ (numArg n) ++" argument of "++(show op)++":\n    Expected Type: "++(show t)++"\n    Actual Type: "++(show aT))
    numArg 1 = "1st"
    numArg 2 = "2nd"
    numArg 3 = "3rd"
    numArg n = (show n)++"th"
    
    

--Variable Type Frame Helpers
getType name [] = error ("Undefined variable: "++ show name)
getType name (f:fs) = 
  if (fst f)==name
    then (snd f)
    else getType name fs

setType name val [] = [(name,val)]
setType name val (t@(x,_):xs) = 
  if x == name 
    then ((name,val):xs) 
    else t : (setType name val xs) 


--Transform to Reduced Evaluation Type

--Basic Type Transformations
transformEval (Number f) = N f
transformEval (Char c) = C c
transformEval Tru = B 1
transformEval Fls = B 0

--Predefined functions
transformEval (Addition x y) = A (A (I "~+") (transformEval x)) (transformEval y)
transformEval (Subtraction x y) = A (A (I "~-") (transformEval x)) (transformEval y)
transformEval (Multiplication x y) = A (A (I "~*") (transformEval x)) (transformEval y)
transformEval (Division x y) = A (A (I "~/") (transformEval x)) (transformEval y)
transformEval (Exponentiation x y) = A (A (I "~^") (transformEval x)) (transformEval y)
transformEval (Not x) = A (I "~!") (transformEval x)
transformEval (And x y) = A (A (I "~&&") (transformEval x)) (transformEval y)

transformEval (Or x y) = A (A (I "||") (transformEval x)) (transformEval y)
transformEval (Equals x y) = A (A (I "~==") (transformEval x)) (transformEval y)
transformEval (NotEquals x y) = A (A (I "!=") (transformEval x)) (transformEval y)
transformEval (LessThan x y) = A (A (I "<") (transformEval x)) (transformEval y)
transformEval (LessThanEqual x y) = A (A (I "~<=") (transformEval x)) (transformEval y)
transformEval (GreaterThan x y) = A (A (I ">") (transformEval x)) (transformEval y)
transformEval (GreaterThanEqual x y) = A (A (I ">=") (transformEval x)) (transformEval y)
transformEval (EvalIf x y z) = (A (A (A (I "~if") (transformEval x)) (transformEval y)) (transformEval z))
transformEval (Pair x y) = A (A (I "pair") (transformEval x)) (transformEval y)
transformEval (Head x) = A (I "head") (transformEval x)
transformEval (Tail x) = A (I "tail") (transformEval x)
transformEval (Concat x y) = A (A (I "++") (transformEval x)) (transformEval y)

transformEval (Map f ls) = A (A (I "map") (transformEval f)) (transformEval ls)

transformEval (Return x) = transformEval x

transformEval (Variable name) = I name
transformEval Null = I "null"


transformEval (App f@(FuncPtr fName) args) = helper (reverse args)
  where helper [x] = (A (I fName) (transformEval x))
        helper (x:xs) = A (helper xs) (transformEval x)

transformEval (Lambda n sepxr) = (L n (transformEval sepxr))
transformEval (App sexpr [arg]) = (A (transformEval sexpr) (transformEval arg))