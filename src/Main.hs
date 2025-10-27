-- MIT License
-- 
-- Copyright (c) 2025 Luca Padovani
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- |This module parses the command-line arguments and invokes the type checker.
module Main (main) where

import qualified Instrumenter
import qualified Resolver
import qualified Checker
import qualified Solver
import Atoms
import Measure
import Strategy
import Render
import Exceptions (MyException)
import Parser (parseProcess)
import Process
import Type
import System.Console.GetOpt
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import Control.Monad (forM_, unless, when)
import Control.Exception (catch)
import qualified Data.Version
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath.Posix (takeFileName)

import qualified Data.Set as Set
import qualified Data.Map as Map

-- |Version of the program.
version :: Data.Version.Version
version = Data.Version.makeVersion [1, 0]

-- |Entry point.
main :: IO ()
main = do
  progName <- getProgName
  (args, file) <- getArgs >>= parse progName
  source <- if file == "-" then getContents else readFile file
  case parseProcess file source of
    Left msg -> printWarning msg
    Right (tdefs, pdefs) ->
      let pdefs' = Resolver.resolve tdefs pdefs in
        catch (check file args pdefs') (handler args)
  where
    check :: FilePath -> [Flag] -> [ProcessDefS] -> IO ()
    check file args pdefs0 = do
      let verbose = Verbose `elem` args
      let logging = Logging `elem` args
      let manualI = ManualI `elem` args
      let strat =
            Strategy { mcall  = if OnlyCall `elem` args then msucc else id
                     , mlink  = if OnlyCall `elem` args || FreeLink `elem` args then id else msucc
                     , mclose = if OnlyCall `elem` args || FreeClose `elem` args then id else msucc
                     , mfork  = if OnlyCall `elem` args || FreeFork `elem` args then id else msucc
                     , mtag   = if OnlyCall `elem` args || FreeSelect `elem` args then id else msucc
                     , mput   = if OnlyCall `elem` args || FreePut `elem` args then id else msucc }
      when logging
        (do putStr $ takeFileName file ++ " ... "
            hFlush stdout)
      start <- getCurrentTime
      let pdefs = if manualI then pdefs0 else Instrumenter.instrument pdefs0
      let (cs0, pdefs'') = Checker.checkTypes strat pdefs
      -- let (σ, cs1) = Measure.gatherSubstitutions cs0
      -- let cs = map (subst σ) cs1
      -- let pdefs' = map (substProcessDef σ) pdefs''
      let cs = cs0
      let pdefs' = pdefs''
      forM_ pdefs' printProcessDec
      when verbose (forM_ cs (\c -> putStrLn $ "  " ++ show c))
      when True
        (do let μs = Set.toList (mv cs)
            case Solver.solve μs cs of
              Nothing -> printNO "termination checker"
              Just μmap -> do printSolution μmap
                              putStrLn ""
                              printOK (Just "termination checker")
        )
      stop <- getCurrentTime
      when logging $ printOK (Just (show (diffUTCTime stop start)))
      -- case Solver.solve pdefs cs of
      --   Nothing -> printNO "fair termination checker failed"
      --   Just pdefs' -> forM_ pdefs' printProcessDec

    handler :: [Flag] -> MyException -> IO ()
    handler _ e = printNO (show e)

-- |Representation of supported flags.
data Flag = Verbose  -- -v --verbose
          | Version  -- -V --version
          | Logging  --    --log
          | FreePut  -- -p --free-put
          | FreeClose
          | FreeLink
          | FreeFork
          | FreeSelect
          | OnlyCall -- -c --only-call
          | ManualI  -- -i --disable-instrumentation
          | Help     --    --help
            deriving (Eq, Ord)

-- |List of supported flags.
flags :: [OptDescr Flag]
flags =
   [ Option []  ["log"]                     (NoArg Logging)    "Log type checking time"
   , Option "v" ["verbose"]                 (NoArg Verbose)    "Print type checking and running activities"
   , Option "V" ["version"]                 (NoArg Version)    "Print version information"
   , Option "h" ["help"]                    (NoArg Help)       "Print this help message"
   , Option "c" ["free-close"]              (NoArg FreeClose)  "Close operations cost nothing"
   , Option "p" ["free-put"]                (NoArg FreePut)    "Put operations cost nothing"
   , Option "l" ["free-link"]               (NoArg FreeLink)   "Link operations cost nothing"
   , Option "f" ["free-fork"]               (NoArg FreeFork)   "Fork operations cost nothing"
   , Option "s" ["free-select"]             (NoArg FreeSelect) "Select operations cost nothing"
   , Option "r" ["just-call"]               (NoArg OnlyCall)   "Only process invocations are measured"
   , Option "i" ["disable-instrumentation"] (NoArg ManualI)    "Disable automatic instrumentation"]

-- |The information displayed when the verbose option is specified.
versionInfo :: String -> String
versionInfo progName =
  "LInFA " ++ Data.Version.showVersion version ++ " Copyright © 2024 Luca Padovani\n"
  ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  ++ "This is free software: you are free to change and redistribute it.\n"
  ++ "There is NO WARRANTY, to the extent permitted by law."

-- |Parse command-line arguments.
parse :: String -> [String] -> IO ([Flag], String)
parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (Version `elem` args)
        (do hPutStrLn stderr (versionInfo progName)
            exitWith ExitSuccess)
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
