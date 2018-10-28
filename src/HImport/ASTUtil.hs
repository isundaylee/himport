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
  , applyRewrite
  , applyRewrites
  , Rewrite(..)
  )
where
import           Debug.Trace                    ( trace )
import           Control.Monad.State            ( State(..)
                                                , runState
                                                )
import           Data.Generics.Aliases          ( mkQ
                                                , mkT
                                                )
import           Data.Generics.Schemes          ( everythingWithContext
                                                , everything
                                                , everywhere
                                                )
import           Data.Data                      ( Data )
import qualified HImport.Util                  as Util
                                                ( isIdentQualified
                                                , splitTokens
                                                , ImportObject(..)
                                                )
import qualified Language.Haskell.Exts.SrcLoc  as SrcLoc
                                                ( SrcSpanInfo(SrcSpanInfo)
                                                , SrcSpan(SrcSpan)
                                                , SrcLoc(..)
                                                , SrcSpanInfo(..)
                                                , SrcSpan(..)
                                                )
import qualified Language.Haskell.Exts.Syntax  as Syntax
                                                ( ImportDecl(..)
                                                , ImportSpec(..)
                                                , ImportSpecList(..)
                                                , ModuleName(..)
                                                , Name(..)
                                                , QName(..)
                                                , Annotated
                                                , ann
                                                , Module(..)
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( intercalate
                                                , sort
                                                )

type ImportDecl = Syntax.ImportDecl SrcLoc.SrcSpanInfo

type ImportSpec = Syntax.ImportSpec SrcLoc.SrcSpanInfo

type ImportSpecList = Syntax.ImportSpecList SrcLoc.SrcSpanInfo

type ModuleName = Syntax.ModuleName SrcLoc.SrcSpanInfo

type Name = Syntax.Name SrcLoc.SrcSpanInfo

type QName = Syntax.QName SrcLoc.SrcSpanInfo

data Rewrite = Rewrite SrcLoc.SrcSpan Int
             deriving (Show, Eq, Ord)

dummySrcSpanInfo :: SrcLoc.SrcSpanInfo
dummySrcSpanInfo = SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) []

dummySrcLoc :: SrcLoc.SrcLoc
dummySrcLoc = SrcLoc.SrcLoc "" 0 0

advanceSrcLoc :: Int -> SrcLoc.SrcLoc -> SrcLoc.SrcLoc
advanceSrcLoc offset (SrcLoc.SrcLoc filename line col) =
  SrcLoc.SrcLoc filename line (col + offset)

buildSrcSpanInfo :: SrcLoc.SrcLoc -> Int -> SrcLoc.SrcSpanInfo
buildSrcSpanInfo (SrcLoc.SrcLoc filename startLine startCol) len =
  SrcLoc.SrcSpanInfo
    (SrcLoc.SrcSpan filename startLine startCol startLine (startCol + len))
    []

buildName :: String -> SrcLoc.SrcLoc -> Name
buildName name loc = Syntax.Ident (buildSrcSpanInfo loc $ length name) name

buildModuleName :: String -> SrcLoc.SrcLoc -> ModuleName
buildModuleName name loc =
  Syntax.ModuleName (buildSrcSpanInfo loc $ length name) name

buildImportSpec :: Util.ImportObject -> ImportSpec
buildImportSpec (Util.ImportVar  var) = buildIVar var
buildImportSpec (Util.ImportType var) = buildIThingAll var

buildIVar :: String -> ImportSpec
buildIVar name = Syntax.IVar dummySrcSpanInfo $ buildName name dummySrcLoc

buildIThingAll :: String -> ImportSpec
buildIThingAll name =
  Syntax.IThingAll dummySrcSpanInfo $ buildName name dummySrcLoc

buildQName :: String -> SrcLoc.SrcLoc -> QName
buildQName name startLoc
  | not $ Util.isIdentQualified name
  = let (SrcLoc.SrcLoc filename startLine startCol) = startLoc
    in  Syntax.UnQual (buildSrcSpanInfo startLoc $ length name)
          $ buildName name startLoc
  | otherwise
  = let tokens     = Util.splitTokens name
        base       = last tokens
        moduleName = intercalate "." $ init tokens
    in  Syntax.Qual
          (buildSrcSpanInfo startLoc $ length name)
          (buildModuleName moduleName startLoc)
          (buildName base $ advanceSrcLoc (1 + length moduleName) startLoc)

buildUnqualifiedImportDecl :: String -> ImportSpecList -> ImportDecl
buildUnqualifiedImportDecl moduleName specList = Syntax.ImportDecl
  dummySrcSpanInfo
  (buildModuleName moduleName dummySrcLoc)
  False
  False
  False
  Nothing
  Nothing
  (Just specList)

buildQualifiedImportDecl :: String -> String -> ImportSpecList -> ImportDecl
buildQualifiedImportDecl moduleName asName specList = Syntax.ImportDecl
  dummySrcSpanInfo
  (buildModuleName moduleName dummySrcLoc)
  True
  False
  False
  Nothing
  (Just $ buildModuleName asName dummySrcLoc)
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
getStringSpecName (Syntax.IVar _ name  ) = Just $ getStringName name
getStringSpecName (Syntax.IAbs _ _ name) = Just $ getStringName name
getStringSpecName (Syntax.IThingAll _ name) =
  Just $ getStringName name ++ "(..)"
getStringSpecName _ = Nothing

getStringImportObject :: Util.ImportObject -> String
getStringImportObject (Util.ImportVar  var) = var
getStringImportObject (Util.ImportType var) = var ++ "(..)"

specListWithNewSpec :: Util.ImportObject -> ImportSpecList -> ImportSpecList
specListWithNewSpec object (Syntax.ImportSpecList annotation hiding specs) =
  Syntax.ImportSpecList annotation hiding (specs ++ [buildImportSpec object])

spanIsContainedIn :: SrcLoc.SrcSpan -> SrcLoc.SrcSpan -> Bool
spanIsContainedIn spanA spanB =
  let (SrcLoc.SrcSpan _ startLineA startColA endLineA endColA) = spanA
      (SrcLoc.SrcSpan _ startLineB startColB endLineB endColB) = spanB
  in  ((startLineB, startColB) <= (startLineA, startColA))
        && ((endLineB, endColB) >= (endLineA, endColA))

spanPrecedes :: SrcLoc.SrcSpan -> SrcLoc.SrcSpan -> Bool
spanPrecedes spanA spanB = (endLineA, endColA) <= (startLineB, startColB)
 where
  (SrcLoc.SrcSpan nameA startLineA startColA endLineA endColA) = spanA
  (SrcLoc.SrcSpan nameB startLineB startColB endLineB endColB) = spanB

applyContainedRewrite :: SrcLoc.SrcSpan -> Rewrite -> SrcLoc.SrcSpan
applyContainedRewrite span rewrite
  | rewriteLine == endLine = SrcLoc.SrcSpan name
                                            startLine
                                            startCol
                                            endLine
                                            (endCol + rewriteDelta)
  | otherwise = span
 where
  (SrcLoc.SrcSpan name startLine startCol endLine endCol) = span
  (Rewrite rewriteSpan rewriteDelta                     ) = rewrite
  (SrcLoc.SrcSpan _ rewriteLine _ _ _                   ) = rewriteSpan

applyPrecedingRewrite :: SrcLoc.SrcSpan -> Rewrite -> SrcLoc.SrcSpan
applyPrecedingRewrite span rewrite = SrcLoc.SrcSpan name
                                                    startLine
                                                    newStartCol
                                                    endLine
                                                    newEndCol
 where
  (SrcLoc.SrcSpan name startLine startCol endLine endCol    ) = span
  (Rewrite (SrcLoc.SrcSpan _ rewriteLine _ _ _) rewriteDelta) = rewrite
  newStartCol = startCol + if rewriteLine == startLine then rewriteDelta else 0
  newEndCol   = endCol + if rewriteLine == endLine then rewriteDelta else 0

applyRewrite
  :: Syntax.Module SrcLoc.SrcSpanInfo
  -> Rewrite
  -> Syntax.Module SrcLoc.SrcSpanInfo
applyRewrite tree rewrite@(Rewrite rewriteSpan rewriteOffset) = everywhere
  (mkT transformSpan)
  tree
 where
  transformSpan :: SrcLoc.SrcSpan -> SrcLoc.SrcSpan
  transformSpan span
    | rewriteSpan `spanIsContainedIn` span = applyContainedRewrite span rewrite
    | rewriteSpan `spanPrecedes` span      = applyPrecedingRewrite span rewrite
    | otherwise                            = span

applyRewrites
  :: Syntax.Module SrcLoc.SrcSpanInfo
  -> [Rewrite]
  -> Syntax.Module SrcLoc.SrcSpanInfo
applyRewrites tree = foldr (flip applyRewrite) tree . sort
