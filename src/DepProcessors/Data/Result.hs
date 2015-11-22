module DepProcessors.Data.Result where

data Result
   -- | Successfully processed dependency definition
   = Ok 
   -- | Some non-specific error
   | GenericError String
   -- | One or more dependencies could not be found
   | DependencyNotFound [String]
   deriving Show
