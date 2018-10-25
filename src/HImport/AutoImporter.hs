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

addEntryToNewImport :: ImportEntry -> [ImportDecl] -> [ImportDecl]
addEntryToNewImport (moduleName, objectName, maybeAs) imports =
  (newImport : imports)
 where
  importSpecList = (ASTUtil.buildImportSpecList [ASTUtil.buildIVar objectName])
  newImport      = case maybeAs of
    Nothing -> ASTUtil.buildUnqualifiedImportDecl moduleName importSpecList
    Just as -> ASTUtil.buildQualifiedImportDecl moduleName as importSpecList

importTargetMatch :: ImportEntry -> ImportDecl -> Bool
importTargetMatch (_, _, Nothing) (Syntax.ImportDecl _ _ False _ _ _ Nothing _)
  = True
importTargetMatch (_, _, (Just as)) (Syntax.ImportDecl _ _ True _ _ _ (Just importAs) _)
  = (as == (ASTUtil.getStringModuleName importAs))
importTargetMatch _ _ = False

isCollapsibleInto :: ImportEntry -> ImportDecl -> Bool
isCollapsibleInto entry imp | isNothing $ Syntax.importSpecs imp = False
                            | otherwise = importTargetMatch entry imp

addEntryToExistingImport :: ImportEntry -> (ImportDecl) -> (Maybe (ImportDecl))
addEntryToExistingImport entry@(_, entryObject, _) imp
  | entry `isCollapsibleInto` imp
  = Just
    $ (imp
        { Syntax.importSpecs = Just $ ASTUtil.specListWithNewSpec
                                 entryObject
                                 (fromJust $ Syntax.importSpecs imp)
        }
      )
  | otherwise
  = Nothing

addEntryToExistingImports :: [ImportDecl] -> ImportEntry -> (Maybe [ImportDecl])
addEntryToExistingImports imports entry = case foldResult of
  Left  imports -> Nothing
  Right imports -> Just imports
 where
  foldResult = foldr
    (\imp result -> case result of
      Left imports -> case addEntryToExistingImport entry imp of
        Nothing          -> Left (imp : imports)
        Just modifiedImp -> Right (modifiedImp : imports)
      Right imports -> result
    )
    (Left [])
    imports

isSatisfiedBy :: ImportEntry -> ImportDecl -> Bool
isSatisfiedBy entry imp
  | isNothing $ Syntax.importSpecs imp
  = False
  | not $ importTargetMatch entry imp
  = False
  | otherwise
  = let
      (_, objectName, _) = entry
      (Syntax.ImportSpecList _ _ importedSpecs) =
        fromJust $ Syntax.importSpecs imp
      importedObjects = catMaybes $ map ASTUtil.getStringSpecName importedSpecs
    in
      (objectName `elem` importedObjects)

addEntry :: [ImportDecl] -> ImportEntry -> [ImportDecl]
addEntry imports entry
  | any (entry `isSatisfiedBy`) imports = imports
  | otherwise = case addEntryToExistingImports imports entry of
    Nothing         -> addEntryToNewImport entry imports
    Just newImports -> newImports

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
            fixedImports = foldl addEntry imports $ map importEntry idents
            fixedModule =
              (Syntax.Module anno maybeHead progma fixedImports decls)
          in
            Pretty.prettyPrint fixedModule
        Parser.ParseFailed _ _ -> code
