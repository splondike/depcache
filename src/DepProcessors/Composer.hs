module DepProcessors.Composer (
   findDefinitions,
   installFromDefinition,
   extractFilePath,
   ComposerDefinition
) where

import DepProcessors.ProcessorHelpers (getDirName, getFileName, executeInDirectory)

findDefinitions :: [FilePath] -> IO [ComposerDefinition]
findDefinitions files = return $ map ComposerDefinition filtered
   where
      filtered = filter ((=="composer.json") . getFileName) files

installFromDefinition :: ComposerDefinition -> IO (Either String ())
installFromDefinition (ComposerDefinition file) = executeInDirectory "composer" args dir
   where
      dir = getDirName file
      args = ["install", "--ignore-platform-reqs"]

extractFilePath :: ComposerDefinition -> FilePath
extractFilePath (ComposerDefinition fp) = fp

data ComposerDefinition = ComposerDefinition FilePath deriving Show
