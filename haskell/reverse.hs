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

options :: [OptDescr Flag]
options =  [ 
             help "Test",
             version "1.0.0",
             option (params {charIdens = ['a'], stringIdens = ["all"], usage = "allero", defaultArg = "hi", argType = "blah"}),
             option (params {charIdens = ['q'], stringIdens = ["question"], usage = "Ask a question"})
           ]

rvrs = map reverse 

main = parseArgs options
       >>= \parsed -> print (getAllIdentifiers parsed)
       >>= \_      -> print (getArgByIden parsed "all")
       >>= \_      -> return (rvrs (parsedNonOpts parsed))
       >>= print 
       >> putStr (info "duh" options) 
       
