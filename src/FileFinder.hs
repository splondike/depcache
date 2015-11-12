module FileFinder (
   listAllFiles
) where

import qualified FileFinder.Git as Git

-- | Returns Just [FilePath] if we can extract a list of files from the given
-- directory, Nothing if we can't.
listAllFiles :: FilePath -> IO (Maybe [FilePath])
listAllFiles targetDir = do
   processable <- Git.isProcessable targetDir
   case processable of
        True -> Git.listAllFiles targetDir >>= return . Just
        False -> return Nothing
