#!/usr/bin/env stack
-- stack --resolver lts-9.11 script

import MinParse 
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Environment 
import System.Exit
import Data.Maybe

data sett  = sett { verbose :: Bool,
                    version :: Bool,
                    help :: Bool,
                    output :: Maybe String
                  } 

type Fal = (sett -> sett)

opts :: [OptDescr ]
opts = [ Option "h" ["help"] ()

       ]

data Flag = Verbose 
          | Version
          | Help
          | Output String
          deriving (Eq, Show)

options :: [OptDescr Flag]
options =   [ Option ['v'] ["verbose"] (NoArg Verbose) "Chatty output",
            Option ['h'] ["help"] (NoArg Falg Help) "Help",
            Option ['o'] ["output"] (OptArg (Output . fromMaybe "stdout") "OUT") "Output"
          ]

rvrs = map reverse 

getString :: Flag -> Maybe String
getString (Output a) = Just a
getString _ = Nothing

extract:: ([Flag], [String]) -> ([String], [String])
extract (a, b) = (catMaybes (map getString a), b)

--proc ([], []) = getContents
                -- >>= putStrLn . rvrs

main = getArgs
       >>= return . parsed . parse options
       >>= return . extract 
       >>= print
