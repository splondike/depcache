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
processDirectory targetDir = do
   files <- getFiles targetDir
   depProcessors <- mapM (buildProcessors files) processors

   forM_ depProcessors printDefinitionCount

   putStrLn ""

   resultsChan <- newChan

   -- Update each definition type concurrently (for speed). For each of the definitions within
   -- a given type update sequentially (the update command may not be thread safe)
   -- Accumulate the results in resultsChan
   forM_ depProcessors $ \(name, processors) -> forkIO $ do
      forM_ processors $ \(filepath, installer) -> do
         result <- installer
         writeChan resultsChan (filepath, result)

   -- Print out the reults as they come in
   let installerCount = foldl (\c  (_, xs) -> c + (length xs)) 0 depProcessors
   results <- forM [1..installerCount] $ \_ -> do
      (filepath, result) <- readChan resultsChan
      case result of
           Left err -> putStrLn $ "Error processing: " ++ filepath ++ "\n\n" ++ err
           Right _ -> putStrLn $ "Processed: " ++ filepath
      return result

   -- Print the final result tally
   let failureCount = length $ filter isLeft results
   let successCount = (length results) - failureCount
   let wereWas count = if count == 1 then "was" else "were"
   putStrLn $ concat [
               "Processed " ++ (show installerCount) ++ " files, ",
               (show successCount) ++ " " ++ (wereWas successCount) ++ " successful and ",
               (show failureCount) ++ " " ++ (wereWas failureCount) ++ " not."]
   
   return ()

printDefinitionCount (name, processors) = do
      let numDefs = (show $ (length processors))
          pluralized = if numDefs == "1" then " definition." else " definitions."
      putStrLn $ "Found " ++ numDefs ++ " " ++ name ++ pluralized

-- | Builds a (name, proccessingResult) pair. processingResult is evaluated
-- later (downloading doesn't happen as part of this function).
buildProcessors files (name, builder) = do
   fileProcessors <- builder files
   return (name, fileProcessors)

getFiles targetDir = do
   maybeFiles <- listAllFiles targetDir
   case maybeFiles of
        Just files -> return files
        Nothing -> return . error $ "Could not extract a list of files from " ++ targetDir ++ " (is it a git repo?)"
