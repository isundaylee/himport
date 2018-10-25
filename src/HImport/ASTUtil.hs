module HImport.ASTUtil
  ( buildName
  , buildQName
  , buildModuleName
  , buildIVar
  , buildImportSpec
  , buildImportSpecList
  , buildUnqualifiedImportDecl
  , buildQualifiedImportDecl
  , specListWithNewSpec
  , getStringName
  , getStringModuleName
  , getStringQName
  , getStringSpecName
  , getStringImportObject
  )
where

import qualified HImport.Util                  as Util
                                                ( isIdentQualified
                                                , splitTokens
                                                , ImportObject(..)
                                                )

import qualified Language.Haskell.Exts.SrcLoc  as SrcLoc
                                                ( SrcSpanInfo(SrcSpanInfo)
                                                , SrcSpan(SrcSpan)
                                                )

import qualified Language.Haskell.Exts.Syntax  as Syntax
                                                ( Name(Ident, Symbol)
                                                , QName(Qual, UnQual, Special)
                                                , ModuleName(ModuleName)
                                                , ImportDecl(ImportDecl)
                                                , ImportSpec(IVar, IThingAll)
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
dummySrcSpanInfo = SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) []

buildName :: String -> Name
buildName = Syntax.Ident dummySrcSpanInfo

buildModuleName :: String -> ModuleName
buildModuleName = Syntax.ModuleName dummySrcSpanInfo

buildImportSpec :: Util.ImportObject -> ImportSpec
buildImportSpec (Util.ImportVar  var) = buildIVar var
buildImportSpec (Util.ImportType var) = buildIThingAll var

buildIVar :: String -> ImportSpec
buildIVar = Syntax.IVar dummySrcSpanInfo . buildName

buildIThingAll :: String -> ImportSpec
buildIThingAll = Syntax.IThingAll dummySrcSpanInfo . buildName

buildQName :: String -> QName
buildQName name
  | not $ Util.isIdentQualified name
  = Syntax.UnQual dummySrcSpanInfo $ buildName name
  | otherwise
  = let tokens     = Util.splitTokens name
        base       = last tokens
        moduleName = intercalate "." $ init tokens
    in  Syntax.Qual dummySrcSpanInfo
                    (buildModuleName moduleName)
                    (buildName base)

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
buildImportSpecList = Syntax.ImportSpecList dummySrcSpanInfo False

getStringName :: Syntax.Name l -> String
getStringName (Syntax.Ident  _ name) = name
getStringName (Syntax.Symbol _ name) = name

getStringModuleName :: Syntax.ModuleName l -> String
getStringModuleName (Syntax.ModuleName _ name) = name

getStringQName :: QName -> String
getStringQName (Syntax.Qual _ (Syntax.ModuleName _ moduleName) name) =
  moduleName ++ "." ++ getStringName name
getStringQName (Syntax.UnQual  _ name) = getStringName name
getStringQName (Syntax.Special _ con ) = "SPECIAL"

getStringSpecName :: ImportSpec -> Maybe String
getStringSpecName (Syntax.IVar _ name) = Just $ getStringName name
getStringSpecName (Syntax.IThingAll _ name) =
  Just $ getStringName name ++ "(..)"
getStringSpecName _ = Nothing

getStringImportObject :: Util.ImportObject -> String
getStringImportObject (Util.ImportVar  var) = var
getStringImportObject (Util.ImportType var) = var ++ "(..)"

specListWithNewSpec :: Util.ImportObject -> ImportSpecList -> ImportSpecList
specListWithNewSpec object (Syntax.ImportSpecList annotation hiding specs) =
  Syntax.ImportSpecList annotation hiding (specs ++ [buildImportSpec object])
