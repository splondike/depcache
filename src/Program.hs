module Program (
   processDirectory
) where

import Data.Either (isLeft)
import Control.Monad (forM_, forM)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan, readChan)

import FileFinder (listAllFiles)
import DepProcessors (processors)

processDirectory :: FilePath -> IO ()
processDirectory targetRepo = do
   repoContents <- getFilesInRepo targetRepo
   depDownloaders <- mapM (buildProcessors repoContents) processors

   forM_ depDownloaders printDownloaderCount
   putStrLn ""

   resultsChan <- newChan
   executeDownloaders depDownloaders resultsChan
   let totalExpectedResults = foldl (\c  (_, xs) -> c + (length xs)) 0 depDownloaders
   results <- accumulateResults resultsChan totalExpectedResults

   printSummary results

   return ()

printSummary results = putStrLn summary
   where
      summary = concat [
                  "Processed " ++ (show totalResults) ++ " files, ",
                  (show successCount) ++ " " ++ (wereWas successCount) ++ " successful and ",
                  (show failureCount) ++ " " ++ (wereWas failureCount) ++ " not."
                ]
      totalResults = length results
      failureCount = length $ filter isLeft results
      successCount = totalResults - failureCount
      wereWas count = if count == 1 then "was" else "were"


accumulateResults resultsChan downloaderCount = 
   forM [1..downloaderCount] $ \_ -> do
      (filepath, result) <- readChan resultsChan
      -- Print out the reults as they come in
      case result of
           Left err -> putStrLn $ "Error processing: " ++ filepath ++ "\n\n" ++ err
           Right _ -> putStrLn $ "Processed: " ++ filepath
      return result

-- | Update each definition type concurrently (for speed). For each of the definitions within
-- a given type update sequentially (the update command may not be thread safe)
-- Accumulate the results in resultsChan
executeDownloaders depDownloaders resultsChan = 
   forM_ depDownloaders $ \(_, downloaders) -> forkIO $
      forM_ downloaders $ \(filepath, downloader) -> do
         result <- downloader
         writeChan resultsChan (filepath, result)

printDownloaderCount (downloaderType, processors) = do
      let numDefs = (show $ (length processors))
          pluralized = if numDefs == "1" then " definition." else " definitions."
      putStrLn $ "Found " ++ numDefs ++ " " ++ downloaderType ++ pluralized

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
