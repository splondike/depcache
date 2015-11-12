module Main where

import System.Environment (getArgs)
import System.Console.GetOpt as GO
import System.Directory as D
import System.Exit(exitFailure)
import Program as P

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
       -- or with processing the deps (and disambiguate whether this is a broken dep,
       -- or some other problem)
       -- TODO: Check for program dependencies (e.g. git, bower)
       -- TODO: Try to make the program more testable by injecting things
       True -> P.processDirectory targetDir
       False -> let message = "targetDir argument was not a directory\n\n"
                in exitWithError $ message

options = [target]
   where
      target = GO.Option "" ["targetDir"] (GO.ReqArg id "directory") "The repository to search for dependency description files"

-- TODO: Specify what each of the failure exit codes mean
usage = GO.usageInfo "Usage: depcache --targetDir=<repo root>\n" options
