module DepProcessors.Npm (
   findDefinitions,
   installFromDefinition,
   extractFilePath,
   NpmDefinition
) where

import qualified DepProcessors.Data.Result as R
import DepProcessors.ProcessorHelpers (getFileName, executeGenericProcessor, GenericProcessorConfig(..))

findDefinitions :: [FilePath] -> IO [NpmDefinition]
findDefinitions files = return $ map NpmDefinition filtered
   where
      filtered = filter ((=="package.json") . getFileName) files

installFromDefinition :: NpmDefinition -> IO R.Result
installFromDefinition (NpmDefinition file) = executeGenericProcessor config file
   where
      config = GenericProcessorConfig {
         commandName = "npm",
         commandArguments = ["install"],
         depNotFoundRegex = "^npm http 404 https://registry.npmjs.org/(.+)"
      }

extractFilePath :: NpmDefinition -> FilePath
extractFilePath (NpmDefinition fp) = fp

data NpmDefinition = NpmDefinition FilePath deriving Show
