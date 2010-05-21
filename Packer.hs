-- |"Executable packer" for Haskell code. This is quite different to
-- those "real" executable packers out there. This reads Haskell
-- source, minimizes it and writes output to given executable
-- file. File is not really compiled so you should first test it with
-- GHC.

module Main where

import System.IO
import System.Process
import qualified Data.ByteString.Lazy.Char8 as B
import Codec.Compression.GZip
import Control.Monad (liftM,when)
import System.Environment (getArgs)
import System.Exit
import Instructions
import HaskellMinimizer

-- |Packs code according to given instructions.
pack :: Instructions -> IO ()
pack ins = do
  minimal <- liftM B.pack $ minimizeFiles ins
  loader <- B.readFile $ loaderFile ins
  outH <- openFile (outFile ins) WriteMode
  let compressed = compressWith params minimal
  B.hPut outH loader
  B.hPut outH compressed
  hClose outH
  putStrLn $ "Success. Wrote " ++
           show ((B.length compressed)+(B.length loader)) ++ " bytes to '" ++
           outFile ins ++ "'."
    where params = defaultCompressParams { compressLevel = bestCompression }

-- |Main function reads input from a given file and does packing.
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error "Usage: packer instructionFile"
  ins <- liftM read (readFile (head args))
  pack ins
  ret <- rawSystem "chmod" ["a+x",outFile ins]
  when (ret /= ExitSuccess) $ error "Running chmod failed? Is this Windows?"
