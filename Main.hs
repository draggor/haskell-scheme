module Main where
import System.Environment
import qualified Scheme

main :: IO ()
main = do args <- getArgs
	  putStrLn (Scheme.readExpr (args !! 0))
