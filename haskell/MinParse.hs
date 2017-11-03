module MinParse (
                  Flag(..),
                  parsed,
                  parsedOpts,
                  nonOpts,
                  parse,
                  flgVal,
                  optVal,
                  putversion,
                  puthelp,
                  displayhelp
                ) where

import System.Console.GetOpt
import Data.Maybe
import System.IO
import System.Exit
import Data.List

data Flag = Verbose 
          | Help String
          | Version String
          | Flg
          | Opt String 
           deriving (Eq, Show)

parsed :: ([a], [String], [String]) -> ([a], [String])
parsed trip = (parsedOpts trip, nonOpts trip)

parsedOpts :: ([a], [String], [String]) -> [a]
parsedOpts (a, _, _) = a

nonOpts :: ([a], [String], [String]) -> [String]
nonOpts (_, a, []) = a
nonOpts (_, _, errs) = errs

parse :: [OptDescr a] -> [String] -> ([a], [String], [String])
parse options args = getOpt RequireOrder options args

optVal :: Flag -> Maybe String
optVal (Opt a) = Just a
optVal _ = Nothing

flgVal :: Flag -> Maybe Bool
flgVal (Flg) = Just True
flgVal _ = Just False

puthelp :: Maybe Flag -> IO ()
puthelp (Just (Help a))= putStrLn a
                   >> exitWith(ExitFailure 1)
puthelp Nothing = putStrLn "No help documentation provided."

putversion :: Maybe Flag -> IO ()
putversion (Just (Version a)) = putStrLn a
                         >> exitWith(ExitFailure 1)
putversion Nothing = putStrLn "Unknown Version"

-- Courtesy Functions
--Take a parsed and find and display help.
displayhelp :: String -> ([Flag], [String]) -> IO ()
displayhelp x (a, _) = puthelp (find (== Help x) a)
diaplyhelp _ (_, _) = putStrLn "Error. No help text provided"

