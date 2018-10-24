module HImport.ASTUtil
  ( buildName
  , buildModuleName
  , buildIVar
  , specListWithNewSpec
  , getStringName
  , getStringModuleName
  , getStringQName
  , getStringSpecName
  , buildQName
  )
where

import qualified HImport.Util                  as Util
                                                ( isIdentQualified
                                                , splitTokens
                                                )

import qualified Language.Haskell.Exts.SrcLoc  as SrcLoc
                                                ( SrcSpanInfo(SrcSpanInfo)
                                                , SrcSpan(SrcSpan)
                                                )

import qualified Language.Haskell.Exts.Syntax  as Syntax
                                                ( Name(Ident, Symbol)
                                                , QName(Qual, UnQual)
                                                , ModuleName(ModuleName)
                                                , ImportSpec(IVar)
                                                , ImportSpecList(ImportSpecList)
                                                )

import           Data.Maybe                     ( catMaybes )

import           Data.List                      ( intercalate )

dummySrcSpanInfo :: SrcLoc.SrcSpanInfo
dummySrcSpanInfo = (SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) [])

buildName :: String -> Syntax.Name SrcLoc.SrcSpanInfo
buildName = Syntax.Ident dummySrcSpanInfo

buildModuleName :: String -> Syntax.ModuleName SrcLoc.SrcSpanInfo
buildModuleName = Syntax.ModuleName dummySrcSpanInfo

buildIVar :: String -> Syntax.ImportSpec SrcLoc.SrcSpanInfo
buildIVar = Syntax.IVar dummySrcSpanInfo . buildName

buildQName :: String -> Syntax.QName SrcLoc.SrcSpanInfo
buildQName name
  | not $ Util.isIdentQualified name
  = (Syntax.UnQual dummySrcSpanInfo $ buildName name)
  | otherwise
  = let tokens     = Util.splitTokens name
        base       = last tokens
        moduleName = intercalate "." $ init tokens
    in  (Syntax.Qual dummySrcSpanInfo
                     (buildModuleName moduleName)
                     (buildName base)
        )

getStringName :: Syntax.Name l -> String
getStringName (Syntax.Ident  _ name) = name
getStringName (Syntax.Symbol _ name) = name

getStringModuleName :: Syntax.ModuleName l -> String
getStringModuleName (Syntax.ModuleName _ name) = name

getStringQName :: Syntax.QName SrcLoc.SrcSpanInfo -> String
getStringQName (Syntax.Qual _ (Syntax.ModuleName _ moduleName) name) =
  moduleName ++ "." ++ (getStringName name)
getStringQName (Syntax.UnQual _ name) = getStringName name
getStringQName node                   = "UNKNOWN"

getStringSpecName :: (Syntax.ImportSpec SrcLoc.SrcSpanInfo) -> Maybe String
getStringSpecName (Syntax.IVar _ name) = Just $ getStringName name
getStringSpecName _                    = Nothing

specListWithNewSpec
  :: String
  -> Syntax.ImportSpecList SrcLoc.SrcSpanInfo
  -> Syntax.ImportSpecList SrcLoc.SrcSpanInfo
specListWithNewSpec object specList
  | Syntax.ImportSpecList annotation hiding specs <- specList
  = if object `elem` (catMaybes $ map getStringSpecName specs)
    then specList
    else Syntax.ImportSpecList annotation hiding (specs ++ [buildIVar object])
