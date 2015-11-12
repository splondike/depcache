module DepProcessors where

import qualified DepProcessors.Npm as Npm
import qualified DepProcessors.Bower as Bower
import qualified DepProcessors.Composer as Composer

type Downloader = (FilePath, IO (Either String ()))

processors :: [(String, [FilePath] -> IO [Downloader])]
processors = [
      ("npm", builder Npm.findDefinitions Npm.installFromDefinition Npm.extractFilePath),
      ("bower", builder Bower.findDefinitions Bower.installFromDefinition Bower.extractFilePath),
      ("composer", builder Composer.findDefinitions Composer.installFromDefinition Composer.extractFilePath)
   ]
   where
      builder defnFinder downloader defFilePath files = do
         defs <- defnFinder files
         return $ map (\def -> (defFilePath def, downloader def)) defs
