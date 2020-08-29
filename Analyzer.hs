module Analyzer (transform,typeCheck) where
import Parser
import Data.List  


--Lambda variable name termThatVariableEquals restOfPrgm




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
    --(Assignment name term) -> (Lambda name (transform [term]) Null)
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

typeCheck Null frame = (IO, frame)

typeCheck Fls frame = (Boolean, frame)
typeCheck Tru frame = (Boolean, frame)

typeCheck (Char x) frame = (Character, frame)
typeCheck (Number x) frame = (Numeric, frame)

typeCheck (Addition x y) frame = expectedTypes [x,y] [Numeric,Numeric] "(+)" Numeric frame
typeCheck (Subtraction x y) frame = expectedTypes [x,y] [Numeric,Numeric] "(-)" Numeric frame
typeCheck (Multiplication x y) frame = expectedTypes [x,y] [Numeric,Numeric] "(*)" Numeric frame
typeCheck (Division x y) frame = expectedTypes [x,y] [Numeric,Numeric] "(/)" Numeric frame
typeCheck (Exponentiation x y) frame = expectedTypes [x,y] [Numeric,Numeric] "(^)" Numeric frame

typeCheck (Not x) frame = expectedTypes [x] [Boolean] "(!)" Boolean frame
typeCheck (And x y) frame = expectedTypes [x,y] [Boolean,Boolean] "(&&)" Boolean frame
typeCheck (Or x y) frame = expectedTypes [x,y] [Boolean,Boolean] "(||)" Boolean frame
typeCheck (Equals x y) frame = expectedTypes [x,y] [tY,tY] "(==)" Boolean frame
  where (tY, yFrame) = typeCheck y frame
typeCheck (LessThanEqual x y) frame = expectedTypes [x,y] [Numeric,Numeric] "(<=)" Boolean frame

typeCheck (EvalIf x y z) frame = expectedTypes [x,y,z] [Boolean, tY, tY] "if" tY frame 
  where (tY, yFrame) = typeCheck y frame

typeCheck a@(Pair x y) frame = expectedTypes (tList a) (take (length (tList a)) $ repeat fstType) "[]" (List fstType) frame
  where tList (Pair a Null) = [a]
        tList (Pair a b) =  a:(tList b)
        (fstType, fstFrame) = typeCheck x frame



typeCheck (Return sexpr) frame = 
  case typeCheck sexpr frame of
    (Unknown, _) -> error ("Unable to infer the type for function")
    (t, newFrame) -> 
      case fTypeUnwrapper cFuncType of
        Unknown -> (t,setType "~cFunc" (setfTypeUnwrapper cFuncType t) newFrame)
        t1 -> 
          if (t==t1)
            then (t1,frame)
            else error ("Return types of function doesn't match")
      where cFuncType = (getType "~cFunc" newFrame) 
  where fTypeUnwrapper (Func x y) = fTypeUnwrapper y
        fTypeUnwrapper y = y 
        setfTypeUnwrapper (Func x y) t = Func x (fTypeUnwrapper y)
        setfTypeUnwrapper Unknown t = t
        
    


typeCheck (FuncPtr fname) frame = ((getType fname frame),frame)
typeCheck (Variable varName) frame = ((getType varName frame),frame)


typeCheck (App (FuncPtr "show") [arg]) frame = ((List Character), frame)
--Function call
typeCheck (App f@(FuncPtr fName) args) frame = expectedTypes args (fTypeUnwrapper fType) fName (fTypeReturnUnwrapper fType) fFrame
  where (fType, fFrame) = typeCheck f frame
        fTypeUnwrapper (Func x y) = x : fTypeUnwrapper y
        fTypeUnwrapper y = [] 
        fTypeReturnUnwrapper (Func x y) = fTypeReturnUnwrapper y
        fTypeReturnUnwrapper y = y 
        typeWrapHelper (x:[]) = x
        typeWrapHelper (x:xs) = Func x (typeWrapHelper xs)

--Definition of functions
typeCheck (App (Lambda funcName sexpr) [f@(Lambda argName sexpr2)]) frame = typeCheck sexpr fFrame
  where (fReturnType, fFrame) = typeCheck f (setType "~cFunc" Unknown frame)
--Definition of Variables
typeCheck (App (Lambda varName sexpr) [val]) frame = typeCheck sexpr (setType varName (fst (typeCheck val frame)) frame)

-- CFUNC NEEDS TO POINT TO CURRENT FUNCTION, and DEFINE FUNCTION NAME TO UNKNOWN
typeCheck (Lambda varName sexpr) frame = 
  case (typeCheck sexpr (setType varName Unknown frame)) of
    (t,newFrame) -> 
      if ((getType varName newFrame)==Unknown)
        then error ("Unused variable: "++(show varName) ++ show newFrame)
        else ((Func (getType varName newFrame) t), newFrame)


--Func call
{-
typeCheck (App (FuncPtr fName) args) frame = 
  case getFrame fName frame of
    (Func argTypes retType) -> expectedTypes argTypes (map typeCheck args) fName retType frame
-}
--Todo convert to Arr Char type
typeCheck (Concat x y) frame = expectedTypes [x,y] [Character,Character] "(++)" Boolean frame





expectedTypes as ts op r frame = expectedTypesHelper as ts op 1 r frame
  where 
    expectedTypesHelper [] _ _ _ r frame = (r,frame)
    expectedTypesHelper (a:as) (t:ts) op n r frame = 
      case t==aT of
        True -> (expectedTypesHelper as ts op (n+1) r aFrame)
        False -> 
          if (aT==Unknown)
            then 
              case a of
                (Variable name) -> (expectedTypesHelper as ts op (n+1) r (setType name t aFrame)) --Type inference for variable 
                _ ->  error errMsg
            else error errMsg
     where (aT, aFrame) = typeCheck a frame
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