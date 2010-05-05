-- |Minimizes Haskell files.

module HaskellMinimizer where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Control.Monad (liftM)
import Data.List

minimizeFiles :: [FilePath] -> FilePath -> IO ()
minimizeFiles fs toFile = do
  a <- readModules fs
  writeFile toFile $ minimize a

readModule :: FilePath -> IO HsModule
readModule fromFile = do
  contents <- readFile fromFile
  case parseModule contents of
    (ParseOk a) -> return a
    ParseFailed loc str -> error $ "Parsing failed at " ++
                           (show $ srcLine loc) ++ ":" ++
                           (show $ srcColumn loc) ++ ": "++
                           str

readModules :: [FilePath] -> IO HsModule
readModules ms = liftM combineModules $ mapM readModule ms

-- |Combines many modules together. Namespaces are not checked!
combineModules :: [HsModule] -> HsModule
combineModules ms = HsModule
                    (getSrc $ head ms)
                    main_mod
                    Nothing
                    (foldl1' union $ map (getImp) ms)
                    (concat $ map getDec ms)
    where getSrc (HsModule x _ _ _ _) = x
          getMod (HsModule _ x _ _ _) = x
          getImp (HsModule _ _ _ xs _) = map cleanImport xs
          getDec (HsModule _ _ _ _ x) = x

-- |Cleans import line info so Eq works "intuitively". Used to union imports.
cleanImport x = x {importLoc = (SrcLoc "dummy" 1 1) }


minimize :: HsModule -> String
minimize m = prettyPrintStyleMode minimumStyle minimumMode $ clean m

minimumStyle = Style { mode = OneLineMode
                     , lineLength = 100
                     , ribbonsPerLine = 1.5
                     }

minimumMode = PPHsMode { classIndent = 0
                       , doIndent = 0
                       , caseIndent = 0
                       , letIndent = 0
                       , whereIndent = 0
                       , onsideIndent = 0
                       , spacing = False
                       , layout = PPNoLayout
                       , linePragmas = False
                       , comments = False
                       }

testClean fs = do
  a <- readModules fs
  putStrLn $ prettyPrint $ clean a
  
-- |Clean is no-op at the moment.
clean = id