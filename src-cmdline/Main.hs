module Main where

import System.Environment (getArgs)
import System.Console.GetOpt as GO
import System.Directory as D
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))

import Program as P

main :: IO ()
main = do
   rawArgs <- getArgs
   case GO.getOpt GO.Permute options rawArgs of
        ([targetDir], _, []) -> verifyAndProcess targetDir
        ([], _, []) -> exitWithError ""
        (_, _, errs) -> exitWithError $ concat errs

exitWithError s = putStr (s ++ usage) >> (exitWith $ ExitFailure 1)

verifyAndProcess targetDir = do
   exists <- D.doesDirectoryExist targetDir
   case exists of
       True -> do
          result <- P.processDirectory P.defaultConfig targetDir
          case result of
              P.Ok -> return ()
              P.GenericError -> exitWith $ ExitFailure 1
              P.DependencyNotFound -> exitWith $ ExitFailure 2
       False -> let message = "targetDir argument was not a directory\n\n"
                in exitWithError $ message

options = [target]
   where
      target = GO.Option "" ["targetDir"] (GO.ReqArg id "directory") "The repository to search for dependency description files"

-- TODO: Specify what each of the failure exit codes mean
usage = GO.usageInfo "Usage: depcache --targetDir=<repo root>\nReturn code 0 indicates success, 1 general failure, 2 is failure to find dependency\n" options
