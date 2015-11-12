module DepProcessors.FileHelpers (
   getFileName,
   getDirName,
   executeInDirectory
) where

import System.Exit as SE
import System.Process (createProcess, waitForProcess, proc, CreateProcess(..), StdStream(CreatePipe), terminateProcess, ProcessHandle(..))
import System.IO (hGetContents, hWaitForInput, hGetChar, hReady, Handle(..))
import qualified System.IO.Error as SIOE
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.MVar as MV

-- | Extracts the file name from the given FilePath
getFileName :: FilePath -> String
getFileName path = reverse $ takeWhile (/='/') $ reverse path

-- | Extracts the directory name from the given FilePath
--
-- e.g. /home/normal/dir/file -> /home/normal/dir
getDirName :: FilePath -> String
getDirName path = reverse $ tail $ dropWhile (/='/') $ reverse path

-- | Runs the given executable, with the given args against the given
-- directory.
-- 
-- Times out after 5 minutes of stdout inactivity returning Left with an appropriate
-- message
executeInDirectory :: String -> [String] -> FilePath -> IO (Either String ())
executeInDirectory executable args dir = do
   (_, Just stdOut, Just stdErr, procHandle) <- createProcess execProc
   resultMVar <- MV.newEmptyMVar
   forkIO $ waitForProcess procHandle >>= (MV.tryPutMVar resultMVar) . Just >> return ()
   forkIO $ do
      timedOut <- waitForTimeout stdOut
      if timedOut
         then terminateProcess procHandle >> MV.tryPutMVar resultMVar Nothing >> return ()
         else return ()

   result <- MV.takeMVar resultMVar
   case result of
        Nothing -> return $ Left "Timed out after 5 minutes of inactivity"
        Just exitCode -> extractResult exitCode stdErr
   where
      extractResult SE.ExitSuccess _ = return $ Right ()
      extractResult (SE.ExitFailure _) stderr = hGetContents stderr >>= (return . Left)
      execProc = (proc executable args) {
         cwd = Just dir,
         std_out = CreatePipe,
         std_err = CreatePipe
      }

-- | Waits for more than 5 minutes to elapse between output to stdOut.
-- returns True if the handle times out, and False if it is closed before a timeout
waitForTimeout :: Handle -> IO Bool
waitForTimeout stdOut = do
   let processTimeout = 5 * 60 * 1000
   eitherSucceeded <- SIOE.tryIOError (hWaitForInput stdOut processTimeout)
   case eitherSucceeded of
        Left e ->
           if SIOE.isEOFError e
              then return False
              else SIOE.ioError e
        Right True -> emptyHandle stdOut >> waitForTimeout stdOut
        Right False -> return True

-- | Removes all the pending characters from handle
emptyHandle :: Handle -> IO ()
emptyHandle handle = do
   moreInput <- hReady handle
   case moreInput of
        True -> hGetChar handle >> emptyHandle handle
        False -> return ()
