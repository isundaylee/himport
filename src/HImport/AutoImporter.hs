module HImport.AutoImporter
  ( autoImport
  )
where

import           HImport.Util                   ( isIdentQualified
                                                , importedName
                                                , importVarEntry
                                                , importTypeEntry
                                                , splitTokens
                                                , ImportEntry
                                                )

import qualified HImport.ASTUtil               as ASTUtil
                                                ( buildQName
                                                , buildImportSpec
                                                , buildIVar
                                                , buildImportSpecList
                                                , buildUnqualifiedImportDecl
                                                , buildQualifiedImportDecl
                                                , getStringSpecName
                                                , getStringQName
                                                , getStringModuleName
                                                , specListWithNewSpec
                                                , getStringImportObject
                                                , Rewrite(..)
                                                )

import           Debug.Trace                    ( trace )

import qualified Language.Haskell.Exts.Parser  as Parser
import qualified Language.Haskell.Exts.Syntax  as Syntax
import qualified Language.Haskell.Exts.SrcLoc  as SrcLoc
import qualified Language.Haskell.Exts.Pretty  as Pretty
import qualified Language.Haskell.Exts.Build   as Build
import qualified Language.Haskell.Exts.ExactPrint
                                               as ExactPrint

import           Data.Generics.Schemes          ( everywhere
                                                , everywhereM
                                                , everythingWithContext
                                                )
import           Data.Generics.Aliases          ( mkT
                                                , mkM
                                                , mkQ
                                                , extQ
                                                )
import           Data.Data                      ( Data )
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                , fromMaybe
                                                , isNothing
                                                , catMaybes
                                                , mapMaybe
                                                )

import           Control.Monad.State            ( State
                                                , runState
                                                , get
                                                , put
                                                )

import           Data.Char                      ( toUpper )

import           Data.Text.Lazy                 ( unpack )

import           Text.Show.Prettyprint          ( prettyShow )

type ImportDecl = Syntax.ImportDecl SrcLoc.SrcSpanInfo
type ModuleName = Syntax.ModuleName SrcLoc.SrcSpanInfo
type QName = Syntax.QName SrcLoc.SrcSpanInfo
type Type = Syntax.Type SrcLoc.SrcSpanInfo
type Exp = Syntax.Exp SrcLoc.SrcSpanInfo
type Pat = Syntax.Pat SrcLoc.SrcSpanInfo

--------------------------------------------------------------------------------
-- Add-to-import logic
--------------------------------------------------------------------------------

addEntryToNewImport :: ImportEntry -> [ImportDecl] -> [ImportDecl]
addEntryToNewImport (moduleName, object, maybeAs) imports = newImport : imports
 where
  importSpecList = ASTUtil.buildImportSpecList [ASTUtil.buildImportSpec object]
  newImport      = case maybeAs of
    Nothing -> ASTUtil.buildUnqualifiedImportDecl moduleName importSpecList
    Just as -> ASTUtil.buildQualifiedImportDecl moduleName as importSpecList

importTargetMatch :: ImportEntry -> ImportDecl -> Bool
importTargetMatch (_, _, Nothing) (Syntax.ImportDecl _ _ False _ _ _ Nothing _)
  = True
importTargetMatch (_, _, Just as) (Syntax.ImportDecl _ _ True _ _ _ (Just importAs) _)
  = as == ASTUtil.getStringModuleName importAs
importTargetMatch _ _ = False

isCollapsibleInto :: ImportEntry -> ImportDecl -> Bool
isCollapsibleInto entry@(entryModule, _, _) imp
  | isNothing $ Syntax.importSpecs imp = False
  | not $ importTargetMatch entry imp = False
  | Syntax.importQualified imp = True
  | otherwise = (==) entryModule
                     (ASTUtil.getStringModuleName (Syntax.importModule imp))

addEntryToExistingImport :: ImportEntry -> ImportDecl -> Maybe ImportDecl
addEntryToExistingImport entry@(_, entryObject, _) imp
  | entry `isCollapsibleInto` imp = Just
    (imp
      { Syntax.importSpecs = Just $ ASTUtil.specListWithNewSpec
                               entryObject
                               (fromJust $ Syntax.importSpecs imp)
      }
    )
  | otherwise = Nothing

addEntryToExistingImports :: [ImportDecl] -> ImportEntry -> Maybe [ImportDecl]
addEntryToExistingImports imports entry = case foldResult of
  Left  imports -> Nothing
  Right imports -> Just imports
 where
  foldResult = foldr
    (\imp result -> case result of
      Left imports -> case addEntryToExistingImport entry imp of
        Nothing          -> Left (imp : imports)
        Just modifiedImp -> Right (modifiedImp : imports)
      Right imports -> Right (imp : imports)
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
  = let (_, objectName, _) = entry
        Syntax.ImportSpecList _ _ importedSpecs =
          fromJust $ Syntax.importSpecs imp
        importedObjects = mapMaybe ASTUtil.getStringSpecName importedSpecs
    in  elem (ASTUtil.getStringImportObject objectName) importedObjects

addEntry :: [ImportDecl] -> ImportEntry -> [ImportDecl]
addEntry imports entry
  | any (entry `isSatisfiedBy`) imports = imports
  | otherwise = fromMaybe (addEntryToNewImport entry imports)
                          (addEntryToExistingImports imports entry)

zeroOutSrcSpanInfo :: Data a => a -> a
zeroOutSrcSpanInfo = everywhere $ mkT visit
 where
  visit :: SrcLoc.SrcSpanInfo -> SrcLoc.SrcSpanInfo
  visit info = SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) []

data CollectFlag = Var | Type | Skip;
data IdentList = IdentList [String] [String]

mergeIdentList :: IdentList -> IdentList -> IdentList
mergeIdentList (IdentList va ta) (IdentList vb tb) =
  IdentList (va ++ vb) (ta ++ tb)

collectIdents :: Data a => a -> IdentList
collectIdents tree = IdentList (filter isIdentQualified allVarIdents)
                               (filter isIdentQualified allTypeIdents)
 where
  emptyIdentList = IdentList [] []

  defaultOp :: [CollectFlag] -> (IdentList, [CollectFlag])
  defaultOp flags = (emptyIdentList, Var : flags)

  (IdentList allVarIdents allTypeIdents) = everythingWithContext
    []
    mergeIdentList
    (extQ (extQ (extQ (mkQ defaultOp markType) markExp) markPat) collect)
    tree

  markType :: Type -> [CollectFlag] -> (IdentList, [CollectFlag])
  markType (Syntax.TyCon _ _) flags = (emptyIdentList, Type : flags)
  markType _                  flags = (emptyIdentList, Var : flags)

  markExp :: Exp -> [CollectFlag] -> (IdentList, [CollectFlag])
  markExp (Syntax.Con _ _) flags = (emptyIdentList, Skip : flags)
  markExp _                flags = (emptyIdentList, Var : flags)

  markPat :: Pat -> [CollectFlag] -> (IdentList, [CollectFlag])
  markPat Syntax.PApp{} flags = (emptyIdentList, Skip : flags)
  markPat _             flags = (emptyIdentList, Var : flags)

  collect :: QName -> [CollectFlag] -> (IdentList, [CollectFlag])
  collect qName flags@(Type : _) =
    (IdentList [] [ASTUtil.getStringQName qName], flags)
  collect qName flags@(Var : _) =
    (IdentList [ASTUtil.getStringQName qName] [], flags)
  collect qName flags@(Skip : _) = (emptyIdentList, flags)
  collect _     flags            = (emptyIdentList, flags)

rewriteIdents :: Data a => a -> (a, [ASTUtil.Rewrite])
rewriteIdents tree = runState (everywhereM (mkM rewrite) tree) []
 where
  rewrite :: QName -> State [ASTUtil.Rewrite] QName
  rewrite node = do
    let oldName = ASTUtil.getStringQName node
    if isIdentQualified oldName
      then do
        let newName = importedName oldName
        let span    = SrcLoc.srcInfoSpan $ Syntax.ann node
        let newNode = ASTUtil.buildQName newName $ SrcLoc.SrcLoc
              (SrcLoc.srcSpanFilename span)
              (SrcLoc.srcSpanStartLine span)
              (SrcLoc.srcSpanStartColumn span)
        let newRewrite = ASTUtil.Rewrite
              (SrcLoc.srcInfoSpan $ Syntax.ann node)
              (length newName - length oldName)
        rewrites <- get
        put $ newRewrite : rewrites
        return newNode
      else return node

autoImport :: String -> String
autoImport code =
  let parseMode = Parser.defaultParseMode
      result    = Parser.parseModuleWithComments parseMode code
  in  case result of
        Parser.ParseOk (tree, comments) ->
          let
            (IdentList varIdents typeIdents) = collectIdents tree
            (fixedTree, rewrites)            = rewriteIdents tree
            (Syntax.Module anno maybeHead progma imports decls) = fixedTree
            fixedImports                     = foldl addEntry imports
              $ (++)
                  (map importVarEntry varIdents)
                  (map importTypeEntry typeIdents)
            fixedModule =
              Syntax.Module anno maybeHead progma fixedImports decls
          in
            Pretty.prettyPrint fixedModule
        Parser.ParseFailed _ _ -> code
