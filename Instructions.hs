-- |A simple data structure for giving instructions to minimizer.

module Instructions where

data Instructions = Instructions {
      inputFiles :: [FilePath]  -- ^Files to read.
    , keepSigs   :: [String]    -- ^Type signatures not to remove.
    , loaderFile :: FilePath    -- ^File to use as a loader.
    , outFile    :: FilePath    -- ^Output to this file.
    } deriving (Show,Read)
