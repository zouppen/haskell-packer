-- |This module shows the internal Haskell structure of the Haskell
-- source code. So it's a Haskell representation of the source
-- code in same language. :-)

module ShowStructure where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

type Shower = (HsModule -> String)

structure :: Shower -> FilePath -> IO String
structure shower f = do
  origText <- readFile f
  let orig = parseSimple origText
  let newText = "a="++shower orig
  return $ prettyPrint $ parseSimple newText
  
parseSimple :: String -> HsModule
parseSimple str =
  case parseModule str of
    (ParseOk a) -> a
    ParseFailed loc str -> error $ "Parsing failed at " ++
                           (show $ srcLine loc) ++ ":" ++
                           (show $ srcColumn loc) ++ ": "++
                           str

getBody :: Shower
getBody (HsModule _ _ _ _ x) = show x

getImports :: Shower
getImports (HsModule _ _ _ x _) = show x