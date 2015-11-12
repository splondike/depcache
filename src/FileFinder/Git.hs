module FileFinder.Git (
   listAllFiles,
   isProcessable
) where

import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))
import qualified System.Posix.Files as SPF
import qualified System.Exit as SE
import qualified System.Directory as SD

-- | Lists all the files in the given repository root directory
listAllFiles :: FilePath -> IO [FilePath]
listAllFiles repoRoot = checkPathValid repoRoot getFiles
   where
      getFiles = readCreateProcessWithExitCode gitProc "" >>= (return . extractResult)
      extractResult (SE.ExitSuccess, stdout, _) = map (normalizedRoot ++) $ lines stdout
      extractResult (SE.ExitFailure _, _, _) = error "Error listing contents with git"
      gitProc = (proc "git" gitArgs) {cwd = Just normalizedRoot}
      gitArgs = ["ls-tree", "--full-tree", "-r", "--name-only", "HEAD"]
      normalizedRoot = normalizeDirectory repoRoot

-- | Is the given root directory suitable for processing by listAllFiles?
isProcessable :: FilePath -> IO Bool
isProcessable repoRoot = do
   let gitDir = repoRoot ++ ".git/"
   SPF.fileExist gitDir
   where
      normalizedRoot = normalizeDirectory repoRoot

checkPathValid :: FilePath -> IO [FilePath] -> IO [FilePath]
checkPathValid repoRoot f = do
   gitDirExists <- isProcessable repoRoot
   if gitDirExists then
      f
   else
      let errMsg = repoRoot ++ " does not have a .git subdirectory" in
      return $ error errMsg

normalizeDirectory dir = (++"/") $ reverse $ dropWhile (=='/') $ reverse dir
