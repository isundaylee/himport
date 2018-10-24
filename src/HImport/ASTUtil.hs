module HImport.ASTUtil
  ( buildName
  , buildQName
  , buildModuleName
  , buildIVar
  , buildImportSpecList
  , buildUnqualifiedImportDecl
  , buildQualifiedImportDecl
  , specListWithNewSpec
  , getStringName
  , getStringModuleName
  , getStringQName
  , getStringSpecName
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
                                                , ImportDecl(ImportDecl)
                                                , ImportSpec(IVar)
                                                , ImportSpecList(ImportSpecList)
                                                )

import           Data.Maybe                     ( catMaybes )

import           Data.List                      ( intercalate )

type ImportDecl = Syntax.ImportDecl SrcLoc.SrcSpanInfo
type ImportSpec = Syntax.ImportSpec SrcLoc.SrcSpanInfo
type ImportSpecList = Syntax.ImportSpecList SrcLoc.SrcSpanInfo
type ModuleName = Syntax.ModuleName SrcLoc.SrcSpanInfo
type Name = Syntax.Name SrcLoc.SrcSpanInfo
type QName = Syntax.QName SrcLoc.SrcSpanInfo

dummySrcSpanInfo :: SrcLoc.SrcSpanInfo
dummySrcSpanInfo = (SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) [])

buildName :: String -> Name
buildName = Syntax.Ident dummySrcSpanInfo

buildModuleName :: String -> ModuleName
buildModuleName = Syntax.ModuleName dummySrcSpanInfo

buildIVar :: String -> ImportSpec
buildIVar = Syntax.IVar dummySrcSpanInfo . buildName

buildQName :: String -> QName
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

buildUnqualifiedImportDecl :: String -> ImportSpecList -> ImportDecl
buildUnqualifiedImportDecl moduleName specList = Syntax.ImportDecl
  dummySrcSpanInfo
  (buildModuleName moduleName)
  False
  False
  False
  Nothing
  Nothing
  (Just specList)

buildQualifiedImportDecl :: String -> String -> ImportSpecList -> ImportDecl
buildQualifiedImportDecl moduleName asName specList = Syntax.ImportDecl
  dummySrcSpanInfo
  (buildModuleName moduleName)
  True
  False
  False
  Nothing
  (Just $ buildModuleName asName)
  (Just specList)

buildImportSpecList :: [ImportSpec] -> ImportSpecList
buildImportSpecList objects =
  Syntax.ImportSpecList dummySrcSpanInfo False objects

getStringName :: Syntax.Name l -> String
getStringName (Syntax.Ident  _ name) = name
getStringName (Syntax.Symbol _ name) = name

getStringModuleName :: Syntax.ModuleName l -> String
getStringModuleName (Syntax.ModuleName _ name) = name

getStringQName :: QName -> String
getStringQName (Syntax.Qual _ (Syntax.ModuleName _ moduleName) name) =
  moduleName ++ "." ++ (getStringName name)
getStringQName (Syntax.UnQual _ name) = getStringName name
getStringQName node                   = "UNKNOWN"

getStringSpecName :: ImportSpec -> Maybe String
getStringSpecName (Syntax.IVar _ name) = Just $ getStringName name
getStringSpecName _                    = Nothing

specListWithNewSpec :: String -> ImportSpecList -> ImportSpecList
specListWithNewSpec object specList
  | Syntax.ImportSpecList annotation hiding specs <- specList
  = if object `elem` (catMaybes $ map getStringSpecName specs)
    then specList
    else Syntax.ImportSpecList annotation hiding (specs ++ [buildIVar object])
