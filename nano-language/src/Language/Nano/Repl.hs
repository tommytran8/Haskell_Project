-- | This module has various "utilities" that you can use to build a REPL. 

module Language.Nano.Repl where

import           Control.Exception
import           System.Exit
import           System.IO
import qualified Data.List as L
import qualified Data.Char as Char
import           Language.Nano.Types 
import           Language.Nano.Eval  

--------------------------------------------------------------------------------
welcome :: String
--------------------------------------------------------------------------------
welcome = unlines
  [ "------------------------------------------------------------"
  , "-------- The NANO Interpreter v.0.0.0.0 --------------------"
  , "------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------
putStrFlush :: String -> IO ()
--------------------------------------------------------------------------------
putStrFlush str = do 
  putStr str
  hFlush stdout

--------------------------------------------------------------------------------
doQuit :: IO a 
--------------------------------------------------------------------------------
doQuit = do 
  putStrLn "Goodbye." 
  exitWith ExitSuccess 

--------------------------------------------------------------------------------
doEval :: Env -> String -> IO ()
--------------------------------------------------------------------------------
doEval env s = (print =<< execEnvString env s) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doUnknown :: IO () 
--------------------------------------------------------------------------------
doUnknown = putStrLn "I'm sorry Dave, I'm sorry I can't do that..."

--------------------------------------------------------------------------------
doRun :: FilePath -> IO ()
--------------------------------------------------------------------------------
doRun f = (print =<< execFile f) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doLoad :: FilePath -> IO Env
--------------------------------------------------------------------------------
doLoad f = (defsEnv =<< defsFile f) `catch` exitEnv 

exitEnv :: Error -> IO Env
exitEnv err = putStrLn (errMsg err) >> return prelude 


--------------------------------------------------------------------------------
-- HINT: You may want to implement `defsEnv` and then use `doLoad`
--------------------------------------------------------------------------------
defsEnv :: [(Id, Expr)] -> IO Env
--------------------------------------------------------------------------------
defsEnv xes = putStrLn ("definitions: " ++ getids r) >> return (r)
	where 
		 r = helper prelude (reverse xes)
helper p []   = reverse p
helper p ((id, exp):xs) = (helper p xs) ++ [(id,eval (helper p xs) exp)] 
getids ((id, _):[]) = id
getids ((id, _):ys) = id ++ " " ++ getids ys


--------------------------------------------------------------------------------
-- | A Datatype Commands for the shell -----------------------------------------
--------------------------------------------------------------------------------

data Cmd 
  = CEval String    -- ^ `CEval s` means parse-and-evaluate the `s`
  | CRun  FilePath  -- ^ `CRun f`  means parse-and-evaluate the "top" binder of `f`
  | CLoad FilePath  -- ^ `CLoad f` means parse-and-add-to-env all the binders of `f`
  | CQuit           -- ^ `CQuit`   means exit the shell
  | CUnknown        -- ^ any other unknown command
  deriving (Show)

strCmd :: Env -> String -> IO ()
strCmd env str = if L.isPrefixOf pfxQuit str
		then doQuit
		else
		  if L.isPrefixOf pfxRun str
		    then doRun (toFilePath 5 (str))
		    else 
			  if L.isPrefixOf ":" str
		  		then  doUnknown
		  		else
					doEval env str
loadCmd :: String -> IO Env
loadCmd str = doLoad (toFilePath 6 (str))

checkLoad :: String -> Bool
checkLoad str = L.isPrefixOf pfxLoad str
toFilePath :: Int -> String -> FilePath
toFilePath n s = drop n s
--removePunc xs = [x | x <- xs, not (x `elem` ":")]
		     

-- HINT: You may want to use the below functions and `L.isPrefixOf`, `chomp`, `pfxRun`, 

chomp :: Int -> String -> String
chomp n s = dropWhile Char.isSpace (drop n s)

pfxRun, pfxLoad, pfxQuit :: String 
pfxRun  = ":run"
pfxLoad = ":load"
pfxQuit = ":quit"


