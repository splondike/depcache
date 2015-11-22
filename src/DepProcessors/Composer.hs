module DepProcessors.Composer (
   findDefinitions,
   installFromDefinition,
   extractFilePath,
   ComposerDefinition
) where

import qualified DepProcessors.Data.Result as R
import DepProcessors.ProcessorHelpers (getFileName, executeGenericProcessor, GenericProcessorConfig(..))

findDefinitions :: [FilePath] -> IO [ComposerDefinition]
findDefinitions files = return $ map ComposerDefinition filtered
   where
      filtered = filter ((=="composer.json") . getFileName) files

installFromDefinition :: ComposerDefinition -> IO R.Result
installFromDefinition (ComposerDefinition file) = executeGenericProcessor config file
   where
      config = GenericProcessorConfig {
         commandName = "composer",
         commandArguments = ["install", "--ignore-platform-reqs"],
         depNotFoundRegex = "The requested package ([^ ]+) could not be found in any version"
      }

extractFilePath :: ComposerDefinition -> FilePath
extractFilePath (ComposerDefinition fp) = fp

data ComposerDefinition = ComposerDefinition FilePath deriving Show
