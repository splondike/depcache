module DepProcessors.Npm (
   findDefinitions,
   installFromDefinition,
   extractFilePath,
   NpmDefinition
) where

import DepProcessors.ProcessorHelpers (getDirName, getFileName, executeInDirectory)

findDefinitions :: [FilePath] -> IO [NpmDefinition]
findDefinitions files = return $ map NpmDefinition filtered
   where
      filtered = filter ((=="package.json") . getFileName) files

installFromDefinition :: NpmDefinition -> IO (Either String ())
installFromDefinition (NpmDefinition file) = executeInDirectory "npm" args dir
   where
      dir = getDirName file
      args = ["install"]

extractFilePath :: NpmDefinition -> FilePath
extractFilePath (NpmDefinition fp) = fp

data NpmDefinition = NpmDefinition FilePath deriving Show
