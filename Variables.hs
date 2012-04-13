-- |This module gets a list of used and defined functions. It can be
-- used to search imported (or undefined) variables from the source.

module Variables where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import HaskellMinimizer (readModule)
import Data.Set (Set)
import qualified Data.Set as S

data SourceInfo = SourceInfo { defined    :: Set HsIdent -- ^ What definitions come out.
                             , referenced :: Set HsIdent -- ^ List of undefined fns.
                             } deriving (Show, Eq)

nothing = SourceInfo S.empty S.empty

undefDecl :: SourceInfo -> HsDecl -> SourceInfo
undefDecl info (HsTypeDecl _ _ _ _) = nothing
undefDecl info (HsDataDecl _ [] name [] inner _) = undefConDecl 
  --        | HsInfixDecl   SrcLoc HsAssoc Int [HsOp]
  --        | HsNewTypeDecl SrcLoc HsContext HsName [HsName] HsConDecl [HsQName]
  --        | HsClassDecl	 SrcLoc HsContext HsName [HsName] [HsDecl]
  --        | HsInstDecl	 SrcLoc HsContext HsQName [HsType] [HsDecl]
  --        | HsDefaultDecl SrcLoc [HsType]
  --        | HsTypeSig	 SrcLoc [HsName] HsQualType
  --        | HsFunBind     [HsMatch]
  --        | HsPatBind	 SrcLoc HsPat HsRhs {-where-} [HsDecl]
  --        | HsForeignImport SrcLoc String HsSafety String HsName HsType
  --        | HsForeignExport SrcLoc String String HsName HsType

body :: HsModule -> [HsDecl]
body (HsModule _ _ _ _ x) = x

--getImports :: Shower
--getImports (HsModule _ _ _ x _) = show x