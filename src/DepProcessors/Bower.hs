module DepProcessors.Bower (
   findDefinitions,
   installFromDefinition,
   extractFilePath,
   BowerDefinition
) where

import DepProcessors.ProcessorHelpers (getDirName, getFileName, executeInDirectory)

findDefinitions :: [FilePath] -> IO [BowerDefinition]
findDefinitions files = return $ map BowerDefinition filtered
   where
      filtered = filter ((=="bower.json") . getFileName) files

installFromDefinition :: BowerDefinition -> IO (Either String ())
installFromDefinition (BowerDefinition file) = executeInDirectory "bower" args dir
   where
      dir = getDirName file
      args = ["install", "--config.interactive=false"]

extractFilePath :: BowerDefinition -> FilePath
extractFilePath (BowerDefinition fp) = fp

data BowerDefinition = BowerDefinition FilePath deriving Show
