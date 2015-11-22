module DepProcessors.Bower (
   findDefinitions,
   installFromDefinition,
   extractFilePath,
   BowerDefinition
) where

import qualified DepProcessors.Data.Result as R
import DepProcessors.ProcessorHelpers (getFileName, executeGenericProcessor, GenericProcessorConfig(..))

findDefinitions :: [FilePath] -> IO [BowerDefinition]
findDefinitions files = return $ map BowerDefinition filtered
   where
      filtered = filter ((=="bower.json") . getFileName) files

installFromDefinition :: BowerDefinition -> IO R.Result
installFromDefinition (BowerDefinition file) = executeGenericProcessor config file
   where
      config = GenericProcessorConfig {
         commandName = "bower",
         commandArguments = ["install", "--config.interactive=false"],
         depNotFoundRegex = "ENOTFOUND Package ([^ ]+) not found"
      }

extractFilePath :: BowerDefinition -> FilePath
extractFilePath (BowerDefinition fp) = fp

data BowerDefinition = BowerDefinition FilePath deriving Show
