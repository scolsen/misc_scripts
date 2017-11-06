#!/usr/bin/env stack
-- stack --resolver lts-6.25 script

import MinParse 
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Environment 
import System.Exit
import Data.Maybe
import Data.List

-- Use Opt for arguments that take strings.
-- Use Flg for arguments that are boolean flags.

version = "1.0.0"
help = "Usage: ..."

options :: [OptDescr Flag]
options =  [ Option ['v'] ["verbose"] (NoArg Verbose) "Chatty output",
             Option ['h'] ["help"] (NoArg (Help help)) "Help",
             Option ['o'] ["output"] (OptArg (Opt ["o"] . fromMaybe "stdout") "OUT") "Output",
             Option ['d'] ["dragon"] (OptArg (Opt ["dragon","d"] . fromMaybe "other") "OUT") "Output"
           ]

rvrs = map reverse 

main = parseArgs options
       >>= \parsed -> print parsed
       >>= \_ -> print (getValByName "d" parsed)
       >>= \_ -> return (rvrs (parsedToNonOpts parsed))
       >>= print
