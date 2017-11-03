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

version :: String
version = "1.0.0"

help :: String
help = "Usage: ..."

options :: [OptDescr Flag]
options =   [ Option ['v'] ["verbose"] (NoArg Verbose) "Chatty output",
            Option ['h'] ["help"] (NoArg (Help help)) "Help",
            Option ['o'] ["output"] (OptArg (Opt . fromMaybe "stdout") "OUT") "Output",
            Option ['d'] ["dragon"] (OptArg (Opt . fromMaybe "other") "OUT") "Output"
          ]

rvrs = map reverse 

extract :: ([Flag], [String]) -> ([String], [String])
extract (a, b) = (catMaybes (map optVal a), b)

main = getArgs
       >>= return . parsed .  parse options
       >>= displayhelp help
       -- >>= return .extract 
       -- >>= print
