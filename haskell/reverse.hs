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
options =  [ noArg ['v'] ["verbose"] True "Chatty output",
             Option ['V'] ["version"] (NoArg (Version version)) "Version",
             Option ['h'] ["help"] (NoArg (Help help)) "Help",
             optArg ['o'] ["output"] "stdout" "OUT" "Output",
             optArg ['d'] ["dragon"] "other" "OUT" "Output",
             optArg ['t'] ["test"] "test" "TET" "this is a test"
           ]

rvrs = map reverse 

main = parseArgs options
       >>= \parsed -> print (getAllIdentifiers parsed)
       >>= \_      -> print (getArgByIden parsed "dragon")
       >>= \_      -> return (rvrs (parsedNonOpts parsed))
       >>= print
       
