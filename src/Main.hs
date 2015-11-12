module Main where

import System.Environment (getArgs)
import System.Console.GetOpt as GO
import System.Directory as D
import System.Exit(exitFailure)
import Program as P
import Debug.Trace

main :: IO ()
main = do
   rawArgs <- getArgs
   case GO.getOpt GO.Permute options rawArgs of
        ([targetDir], _, []) -> verifyAndProcess targetDir
        ([], _, []) -> exitWithError ""
        (_, _, errs) -> exitWithError $ concat errs

exitWithError s = putStr (s ++ usage) >> exitFailure

verifyAndProcess targetDir = do
   exists <- D.doesDirectoryExist targetDir
   case exists of
       -- TODO: Have an exit code here saying whether there was a problem with args
       -- or with processing the deps
       True -> traceShow targetDir $ P.processDirectory targetDir
       False -> let message = "targetDir argument was not a directory\n\n"
                in exitWithError $ message

options = [target]
   where
      target = GO.Option "" ["targetDir"] (GO.ReqArg id "directory") "The repository to search for dependency description files"

usage = GO.usageInfo "Usage: depcache --targetDir=<repo root>\n" options
