module MinParse (
                  Flag(..),
                  Parsed,
                  parsed,
                  parsedOpts,
                  nonOpts,
                  parse,
                  parsedToFlags,
                  parsedToNonOpts,
                  parsedToValues,
                  flgVal,
                  optVal,
                  getOptByName,
                  getOptsByName,
                  putversion,
                  puthelp,
                  displayhelp,
                  parsedIO,
                  parseArgs
                ) where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Control.Monad
import Data.Maybe
import Data.List
import Data.Maybe

data Flag = Verbose 
          | Help String
          | Version String
          | Flg [String]
          | Opt [String] String 
           deriving (Eq, Show)

type OptTuple = ([Flag], [String], [String])
type Parsed = ([Flag], [String])

parse :: [OptDescr Flag] -> [String] -> ([Flag], [String], [String])
parse options args = getOpt RequireOrder options args

parsed :: OptTuple -> Parsed
parsed trip = (parsedOpts trip, nonOpts trip)

parsedIO :: OptTuple -> IO Parsed
parsedIO trip = return (parsedOpts trip, nonOpts trip)

parseArgs :: [OptDescr Flag] -> IO Parsed --Courtesy, getArgs then parse 
parseArgs opts = getArgs
                 >>= parsedIO . parse opts

parsedOpts :: OptTuple -> [Flag]
parsedOpts (a, _, _) = a

nonOpts :: OptTuple -> [String]
nonOpts (_, a, []) = a
nonOpts (_, _, errs) = errs

optVal :: Flag -> Maybe String
optVal (Opt a b) = Just b
optVal _ = Nothing

optNames :: Flag -> [String]
optNames (Opt a b) = a
optNames _ = []

optHasName :: String -> Flag -> Bool
optHasName str (Opt a b) = any (str ==) a
optHasName str (Flg a) = any (str ==) a

flgVal :: Flag -> Maybe Bool
flgVal (Flg a) = Just True
flgVal _ = Just False

optionNames :: [Flag] -> [String] 
optionNames x = join (map optNames x)

getOptByName :: String -> Parsed -> [Flag]
getOptByName str x = filter (optHasName str) (fst x)

getOptsByName :: [String] -> Parsed -> [Flag]
getOptsByName strs p = join (map (\y -> (getOptByName y p)) strs)

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
displayhelp :: String -> Parsed -> IO ()
displayhelp x (a, _) = puthelp (find (== Help x) a)
diaplyhelp _ (_, _) = putStrLn "Error. No help text provided"

parsedToFlags :: Parsed -> [Flag]
parsedToFlags (a, _) = a

parsedToNonOpts :: Parsed -> [String]
parsedToNonOpts (_, b) = b

parsedToValues :: Parsed -> ([String], [String])
parsedToValues (a, b) = (catMaybes (map optVal a), b) 
