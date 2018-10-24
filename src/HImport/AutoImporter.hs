module HImport.AutoImporter
  ( autoImport
  )
where

import           HImport.Util                   ( isIdentQualified
                                                , importedName
                                                , importEntry
                                                , splitTokens
                                                , ImportEntry
                                                )

import qualified HImport.ASTUtil               as ASTUtil
                                                ( buildQName
                                                , buildModuleName
                                                , buildIVar
                                                , buildImportSpecList
                                                , buildUnqualifiedImportDecl
                                                , buildQualifiedImportDecl
                                                , getStringSpecName
                                                , getStringQName
                                                , getStringModuleName
                                                , specListWithNewSpec
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

type ImportDecl = Syntax.ImportDecl SrcLoc.SrcSpanInfo
type ModuleName = Syntax.ModuleName SrcLoc.SrcSpanInfo
type QName = Syntax.QName SrcLoc.SrcSpanInfo

collectAndRewriteIdents :: Data a => a -> (a, [String])
collectAndRewriteIdents tree = runState (everywhereM (mkM visit) tree) []
 where
  visit :: QName -> State [String] (QName)
  visit node = do
    state <- get
    let fullName = ASTUtil.getStringQName node
    put (fullName : state)
    return $ if isIdentQualified fullName
      then ASTUtil.buildQName $ importedName $ fullName
      else node

--------------------------------------------------------------------------------
-- Add-to-import logic
--------------------------------------------------------------------------------

addNewImport :: ImportEntry -> [ImportDecl] -> [ImportDecl]
addNewImport (moduleName, objectName, maybeAs) imports = (newImport : imports)
 where
  importSpecList = (ASTUtil.buildImportSpecList [ASTUtil.buildIVar objectName])
  newImport      = case maybeAs of
    Nothing -> ASTUtil.buildUnqualifiedImportDecl moduleName importSpecList
    Just as -> ASTUtil.buildQualifiedImportDecl moduleName as importSpecList

qualMatch :: (Maybe String) -> Bool -> (Maybe (ModuleName)) -> Bool
qualMatch Nothing False Nothing = True
qualMatch (Just entryAsName) True (Just (Syntax.ModuleName _ importAsName)) =
  (entryAsName == importAsName)
qualMatch _ _ _ = False

addToExistingImport :: ImportEntry -> (ImportDecl) -> (Maybe (ImportDecl))
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
      { Syntax.importSpecs = Just $ ASTUtil.specListWithNewSpec
                               entryObject
                               (fromJust impMaybeSpecs)
      }
    )

addToExistingImports :: ImportEntry -> [ImportDecl] -> (Maybe [ImportDecl])
addToExistingImports ident [] = Nothing
addToExistingImports ident (firstImport : rest) =
  case (addToExistingImport ident firstImport) of
    Just newImport -> Just (newImport : rest)
    Nothing        -> case addToExistingImports ident rest of
      Nothing          -> Nothing
      Just restImports -> Just (firstImport : restImports)

matchInExistingImports :: ImportEntry -> [ImportDecl] -> Bool
matchInExistingImports ident [] = False
matchInExistingImports ident (firstImport : restImports) =
  if match ident firstImport
    then True
    else matchInExistingImports ident restImports
 where
  match :: ImportEntry -> ImportDecl -> Bool
  match (entryModule, entryObject, entryMaybeAs) (Syntax.ImportDecl _ _ importIsQual _ _ _ importMaybeAs importMaybeSpecList)
    | isNothing importMaybeSpecList
    = False
    | (isJust entryMaybeAs) && (isNothing importMaybeAs)
    = False
    | (isNothing entryMaybeAs) && (isJust importMaybeAs)
    = False
    | (isJust entryMaybeAs)
      && (  (fromJust entryMaybeAs)
         /= (ASTUtil.getStringModuleName $ fromJust importMaybeAs)
         )
    = False
    | otherwise
    = entryObject `elem` (catMaybes $ map ASTUtil.getStringSpecName importSpecs)
   where
    (Syntax.ImportSpecList _ _ importSpecs) = fromJust importMaybeSpecList

addImport :: String -> [ImportDecl] -> [ImportDecl]
addImport ident imports =
  let entry   = importEntry ident
      matched = matchInExistingImports entry imports
  in  if matched
        then imports
        else case addToExistingImports entry imports of
          Nothing         -> addNewImport entry imports
          Just newImports -> newImports

addImports :: [String] -> [ImportDecl] -> [ImportDecl]
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
