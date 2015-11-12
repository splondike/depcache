module FileFinder (
   listAllFiles
) where

import qualified FileFinder.Git as Git

listAllFiles :: FilePath -> IO (Maybe [FilePath])
listAllFiles targetDir = do
   processable <- Git.isProcessable targetDir
   case processable of
        True -> Git.listAllFiles targetDir >>= return . Just
        False -> return Nothing
