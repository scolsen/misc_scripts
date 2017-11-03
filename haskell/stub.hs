#!/usr/bin/env stack
-- stack --resolver lts-9.11 script

import System.IO
import Control.Monad
import System.Environment 
import System.Exit
import System.Console.GetOpt
import Data.Maybe

-- Option Helpers
-- NOTE: OptArgs must use java like argument syntax on the command line:
-- e.g. output=blah. Otherwise, the set default will be used.

parsed :: ([a], [String], [String]) -> ([a], [String])
parsed trip = (parsedOpts trip, nonOpts trip)

parsedOpts :: ([a], [String], [String]) -> [a]
parsedOpts (a, _, _) = a

nonOpts :: ([a], [String], [String]) -> [String]
nonOpts (_, a, []) = a
nonOpts (_, _, errs) = errs

data Flag = Verbose 
          | Version
          | Help
          | Output String
          deriving (Eq, Show)

ducks :: [OptDescr Flag]
ducks =   [ Option ['v'] ["verbose"] (NoArg Verbose) "Chatty output",
            Option ['h'] ["help"] (NoArg Help) "Help",
            Option ['o'] ["output"] (OptArg (Output . fromMaybe "stdout") "OUT") "Output"
          ]

parse :: [OptDescr Flag] -> [String] -> ([Flag], [String], [String])
parse options args = getOpt RequireOrder options args

main = getArgs
       >>= return . parsed . parse ducks
       >>= print

