module Program (
   processDirectory,
   defaultConfig,
   LogLevel(..),
   Config(..),
   Result(..)
) where

import Data.List(intercalate)
import Control.Monad (forM_, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan, readChan)

import FileFinder (listAllFiles)
import DepProcessors (processors)
import qualified DepProcessors.Data.Result as R

-- | Finds all the dependency definitions in the given directory and recursively downloads
-- them.
processDirectory ::
      Config -- ^ Configures the operation of the program
   -> FilePath -- ^ The directory to process
   -> IO Result
processDirectory config targetRepo = do
   let logger = (loggerFunc config)
   repoContents <- getFilesInRepo targetRepo
   depDownloaders <- mapM (buildProcessors repoContents) processors

   forM_ depDownloaders (printDownloaderCount logger)

   resultsChan <- newChan
   executeDownloaders depDownloaders resultsChan
   let totalExpectedResults = foldl (\c  (_, xs) -> c + (length xs)) 0 depDownloaders
   results <- accumulateResults logger resultsChan totalExpectedResults

   printSummary logger results

   return $ finalResult results

finalResult = foldl process Ok
   where
      process Ok (R.GenericError _) = GenericError
      process _ (R.DependencyNotFound _) = DependencyNotFound
      process c _ = c

printSummary logger results = logger Info summary
   where
      summary = concat [
                  "Processed " ++ (show totalResults) ++ " files, ",
                  (show successCount) ++ " " ++ (wereWas successCount) ++ " successful and ",
                  (show failureCount) ++ " " ++ (wereWas failureCount) ++ " not."
                ]
      totalResults = length results
      failureCount = length $ filter isFailureResult results
      successCount = totalResults - failureCount
      isFailureResult R.Ok = False
      isFailureResult _ = True
      wereWas count = if count == 1 then "was" else "were"


accumulateResults logger resultsChan downloaderCount =
   forM [1..downloaderCount] $ \_ -> do
      (filepath, result) <- readChan resultsChan
      -- Print out the reults as they come in
      case result of
           R.Ok -> logger Info $ "Processed: " ++ filepath
           R.GenericError err -> logger Error $
             "Error processing: " ++ filepath ++ "\n\n" ++ err
           R.DependencyNotFound deps -> logger Error $
              missingDepsString filepath deps
      return result

missingDepsString filepath missingDeps = out
   where
      out = concat ["Missing dependencies: ", joinedDeps, "\nIn file: ", filepath]
      joinedDeps = intercalate ", " missingDeps

-- | Update each definition type concurrently (for speed). For each of the definitions within
-- a given type update sequentially (the update command may not be thread safe)
-- Accumulate the results in resultsChan
executeDownloaders depDownloaders resultsChan = 
   forM_ depDownloaders $ \(_, downloaders) -> forkIO $
      forM_ downloaders $ \(filepath, downloader) -> do
         result <- downloader
         writeChan resultsChan (filepath, result)

printDownloaderCount logger (downloaderType, processors) = do
      let numDefs = (show $ (length processors))
          pluralized = if numDefs == "1" then " definition." else " definitions."
      logger Notice $ "Found " ++ numDefs ++ " " ++ downloaderType ++ pluralized

-- | Builds a (downloaderType, proccessingResult) pair. processingResult is evaluated
-- later (downloading doesn't happen as part of this function).
buildProcessors files (downloaderType, builder) = do
   fileProcessors <- builder files
   return (downloaderType, fileProcessors)

getFilesInRepo targetDir = do
   maybeFiles <- listAllFiles targetDir
   case maybeFiles of
        Just files -> return files
        Nothing -> return . error $ "Could not extract a list of files from " ++ targetDir ++ " (is it a git repo?)"

-- | Nice defaults for Config
defaultConfig = Config {
   loggerFunc = const putStrLn
}
-- | Describes the importance of a log message
data LogLevel = Notice | Info | Warn | Error deriving (Eq, Ord, Show)
-- | Configuration for the program
data Config = Config {
   loggerFunc :: (LogLevel -> String -> IO ()) -- ^ Function to use as the program logger
}
data Result
   -- | Successfully processed dependency definition
   = Ok
   -- | Some non-specific error
   | GenericError
   -- | A dependency could not be found, the case we're interested in
   | DependencyNotFound
