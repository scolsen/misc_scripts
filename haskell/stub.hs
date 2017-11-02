#!/usr/bin/env stack
-- stack --resolver lts-9.11 script

import System.IO
import Control.Monad
import System.Environment 

reval = map reverse . lines

parseArg :: String -> a 
parseArg arg
    | arg == "-h" = putStrLn "hi" 
    
main = getArgs
       >>= mapM_ parseArgs . words