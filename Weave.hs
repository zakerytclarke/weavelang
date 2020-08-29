import System.Environment   
import Data.List  
import Control.Monad.IO.Class

import Parser
import Analyzer
import Runtime
{-
 -                                               
 - ||   / |  / /                                  
 - ||  /  | / /  ___      ___              ___    
 - || / /||/ / //___) ) //   ) ) ||  / / //___) ) 
 - ||/ / |  / //       //   / /  || / / //        
 - |  /  | / ((____   ((___( (   ||/ / ((____     
 -
 - Weave Programming Language
 - weave.hs
 - Main entry point for weave runtime
 - accepts a single filename as an argument
 - that is parsed, analyzed and executed
 -}
main = do  
  --putStrLn "Weave Programming Language"
  --putStrLn "=============================="
  args <- getArgs --Program path
  if ((length args)/=1)
    then fail "Please specify a single file as an argument to run your program"
    else do
      prgmTxt <- (readFile (head args))
      let ast = (parsePrgm prgmTxt)
      case ast of
        [] -> (error "Unable to parse program")
        ast -> do--Parsed Correctly
                --putStrLn $ show ast
                let trans = (transform ast)
                putStrLn $ show trans
                --eval optimized []
                let typeChecked = (typeCheck trans defaultTypes)
                putStrLn $ show typeChecked
                --case typeChecked of
                  --t -> do
                    --    putStrLn $ show t
                
                
  

defaultTypes =
  [ ("input",(Func (List Character) (List Character))),
    ("print",(Func (List Character) IO))]