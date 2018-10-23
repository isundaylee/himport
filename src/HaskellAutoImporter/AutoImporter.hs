module HaskellAutoImporter.AutoImporter
  ( autoImport
  )
where

import           HaskellAutoImporter.Util       ( isIdentQualified
                                                , importedName
                                                , importEntry
                                                , splitTokens
                                                )

import           Debug.Trace                    ( trace )

import qualified Language.Haskell.Exts.Parser  as Parser
import qualified Language.Haskell.Exts.Syntax  as Syntax
import qualified Language.Haskell.Exts.SrcLoc  as SrcLoc
import qualified Language.Haskell.Exts.Pretty  as Pretty
import qualified Language.Haskell.Exts.Build   as Build

import           Data.Generics.Schemes          ( everywhereM )
import           Data.Generics.Aliases          ( mkM )
import           Data.Data                      ( Data )
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                , isNothing
                                                , catMaybes
                                                )

import           Control.Monad.State            ( State
                                                , runState
                                                , get
                                                , put
                                                )

import           Data.Char                      ( toUpper )

getStringName :: Syntax.Name l -> String
getStringName (Syntax.Ident  _ name) = name
getStringName (Syntax.Symbol _ name) = name

getStringModuleName :: Syntax.ModuleName l -> String
getStringModuleName (Syntax.ModuleName _ name) = name

getFullName :: Syntax.QName SrcLoc.SrcSpanInfo -> String
getFullName (Syntax.Qual _ (Syntax.ModuleName _ moduleName) name) =
  moduleName ++ "." ++ (getStringName name)
getFullName (Syntax.UnQual _ name) = getStringName name
getFullName node                   = "UNKNOWN"

buildQName :: String -> Syntax.QName SrcLoc.SrcSpanInfo
buildQName name
  | not $ isIdentQualified name
  = (Syntax.UnQual dummySrcSpanInfo $ buildName name)
  | otherwise
  = let tokens     = splitTokens name
        base       = last tokens
        moduleName = intercalate "." $ init tokens
    in  (Syntax.Qual dummySrcSpanInfo
                     (buildModuleName moduleName)
                     (buildName base)
        )

collectAndRewriteIdents :: Data a => a -> (a, [String])
collectAndRewriteIdents tree = runState (everywhereM (mkM visit) tree) []
 where
  visit
    :: Syntax.QName SrcLoc.SrcSpanInfo
    -> State [String] (Syntax.QName SrcLoc.SrcSpanInfo)
  visit node = do
    state <- get
    let fullName = getFullName node
    put (fullName : state)
    return $ if isIdentQualified fullName
      then buildQName $ importedName $ fullName
      else node

--------------------------------------------------------------------------------
-- Builder helpers
--------------------------------------------------------------------------------

dummySrcSpanInfo :: SrcLoc.SrcSpanInfo
dummySrcSpanInfo = (SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) [])

buildName :: String -> Syntax.Name SrcLoc.SrcSpanInfo
buildName = Syntax.Ident dummySrcSpanInfo

buildModuleName :: String -> Syntax.ModuleName SrcLoc.SrcSpanInfo
buildModuleName = Syntax.ModuleName dummySrcSpanInfo

buildIVar :: String -> Syntax.ImportSpec SrcLoc.SrcSpanInfo
buildIVar = Syntax.IVar dummySrcSpanInfo . buildName

getSpecName :: (Syntax.ImportSpec SrcLoc.SrcSpanInfo) -> Maybe String
getSpecName (Syntax.IVar _ name) = Just $ getStringName name
getSpecName _                    = Nothing

specListWithNewSpec
  :: String
  -> Syntax.ImportSpecList SrcLoc.SrcSpanInfo
  -> Syntax.ImportSpecList SrcLoc.SrcSpanInfo
specListWithNewSpec object specList@(Syntax.ImportSpecList annotation hiding specs)
  = if object `elem` (catMaybes $ map getSpecName specs)
    then specList
    else Syntax.ImportSpecList annotation hiding (specs ++ [buildIVar object])

--------------------------------------------------------------------------------
-- Add-to-import logic
--------------------------------------------------------------------------------

addNewImport
  :: (String, String, Maybe String)
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
addNewImport (moduleName, objectName, maybeAsName) imports =
  (newImport : imports)
 where
  newSrcSpan     = (SrcLoc.SrcSpan "" 0 0 0 0)
  newSrcSpanInfo = (SrcLoc.SrcSpanInfo newSrcSpan [])
  isQualified    = isJust maybeAsName
  importAs       = case maybeAsName of
    Nothing     -> Nothing
    Just asName -> Just $ (Syntax.ModuleName newSrcSpanInfo asName)
  importSpecs =
    (Syntax.ImportSpecList
      newSrcSpanInfo
      False
      [Syntax.IVar newSrcSpanInfo (Syntax.Ident newSrcSpanInfo objectName)]
    )
  newImport =
    (Syntax.ImportDecl newSrcSpanInfo
                       (Syntax.ModuleName newSrcSpanInfo moduleName)
                       isQualified
                       False
                       False
                       Nothing
                       importAs
                       (Just importSpecs)
    )

qualMatch
  :: (Maybe String)
  -> Bool
  -> (Maybe (Syntax.ModuleName SrcLoc.SrcSpanInfo))
  -> Bool
qualMatch Nothing False Nothing = True
qualMatch (Just entryAsName) True (Just (Syntax.ModuleName _ importAsName)) =
  (entryAsName == importAsName)
qualMatch _ _ _ = False

addToExistingImport
  :: (String, String, Maybe String)
  -> (Syntax.ImportDecl SrcLoc.SrcSpanInfo)
  -> (Maybe (Syntax.ImportDecl SrcLoc.SrcSpanInfo))
addToExistingImport (entryModule, entryObject, entryMaybeAsName) imp@(Syntax.ImportDecl _ (Syntax.ModuleName _ impModule) impIsQual _ _ _ impAs impMaybeSpecs)
  | (isNothing impMaybeSpecs)
  = Nothing
  | impModule /= entryModule
  = Nothing
  | not $ qualMatch entryMaybeAsName impIsQual impAs
  = Nothing
  | otherwise
  = Just
    (imp
      { Syntax.importSpecs = Just $ specListWithNewSpec
                               entryObject
                               (fromJust impMaybeSpecs)
      }
    )

addToExistingImports
  :: (String, String, Maybe String)
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
  -> (Maybe [Syntax.ImportDecl SrcLoc.SrcSpanInfo])
addToExistingImports ident [] = Nothing
addToExistingImports ident (firstImport : rest) =
  case (addToExistingImport ident firstImport) of
    Just newImport -> Just (newImport : rest)
    Nothing        -> case addToExistingImports ident rest of
      Nothing          -> Nothing
      Just restImports -> Just (firstImport : restImports)

matchInExistingImports
  :: (String, String, Maybe String)
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
  -> Bool
matchInExistingImports ident [] = False
matchInExistingImports ident (firstImport : restImports) =
  if match ident firstImport
    then True
    else matchInExistingImports ident restImports
 where
  match
    :: (String, String, Maybe String)
    -> Syntax.ImportDecl SrcLoc.SrcSpanInfo
    -> Bool
  match (entryModule, entryObject, entryMaybeAs) (Syntax.ImportDecl _ _ importIsQual _ _ _ importMaybeAs importMaybeSpecList)
    | isNothing importMaybeSpecList
    = False
    | (isJust entryMaybeAs) && (isNothing importMaybeAs)
    = False
    | (isNothing entryMaybeAs) && (isJust importMaybeAs)
    = False
    | (isJust entryMaybeAs)
      && (  (fromJust entryMaybeAs)
         /= (getStringModuleName $ fromJust importMaybeAs)
         )
    = False
    | otherwise
    = entryObject `elem` (catMaybes $ map getSpecName importSpecs)
   where
    (Syntax.ImportSpecList _ _ importSpecs) = fromJust importMaybeSpecList

addImport
  :: String
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
addImport ident imports =
  let entry   = importEntry ident
      matched = matchInExistingImports entry imports
  in  if matched
        then imports
        else case addToExistingImports entry imports of
          Nothing         -> addNewImport entry imports
          Just newImports -> newImports

addImports
  :: [String]
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
  -> [Syntax.ImportDecl SrcLoc.SrcSpanInfo]
addImports []             imports = imports
addImports (ident : rest) imports = if isIdentQualified ident
  then addImport ident $ addImports rest imports
  else addImports rest imports

autoImport :: String -> String
autoImport code
  = let
      parseMode = Parser.defaultParseMode
      result    = Parser.parseModuleWithMode parseMode code
    in
      case result of
        Parser.ParseOk tree@(Syntax.Module _ _ _ imports _) ->
          let
            (fixedTree, idents) = collectAndRewriteIdents tree
            (Syntax.Module anno maybeHead progma _ decls) = fixedTree
            fixedImports        = addImports idents imports
            fixedModule =
              (Syntax.Module anno maybeHead progma fixedImports decls)
          in
            Pretty.prettyPrint fixedModule
        Parser.ParseFailed _ _ -> code
