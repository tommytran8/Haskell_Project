import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStrLn (printf "%s" welcome)
  loop Nano.prelude 0
loop :: Nano.Env -> Int -> IO ()
loop env n = do
  putStr ("λ ")
  putStrFlush (printf "[%d] " n)
  input <- getLine
  if (checkLoad input) == False
	then do
		strCmd env input
  		loop env (n + 1)
	else do
		envN <- loadCmd input
		loop envN (n + 1)


--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

