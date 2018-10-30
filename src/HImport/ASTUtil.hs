module HImport.ASTUtil
  ( buildName
  , buildQName
  , buildModuleName
  , buildIVar
  , buildIAbs
  , buildIThingAll
  , buildVarName
  , buildConName
  , buildIThingWith
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
  , markImportDecl
  , Rewrite(..)
  )
where
import           Text.Show.Prettyprint          ( prettyShow )
import           Debug.Trace                    ( trace )
import           Control.Monad.State            ( State(..)
                                                , runState
                                                , get
                                                , put
                                                , evalState
                                                , when
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
                                                , CName(..)
                                                , Annotated
                                                , ann
                                                , Module(..)
                                                , Namespace(..)
                                                , importAnn
                                                , importModule
                                                , importAs
                                                , importSpecs
                                                , amap
                                                )
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                , isNothing
                                                , fromJust
                                                )
import           Data.List                      ( intercalate
                                                , sort
                                                )

type ImportDecl = Syntax.ImportDecl SrcLoc.SrcSpanInfo
type ImportSpec = Syntax.ImportSpec SrcLoc.SrcSpanInfo
type ImportSpecList = Syntax.ImportSpecList SrcLoc.SrcSpanInfo
type ModuleName = Syntax.ModuleName SrcLoc.SrcSpanInfo
type Name = Syntax.Name SrcLoc.SrcSpanInfo
type QName = Syntax.QName SrcLoc.SrcSpanInfo
type CName = Syntax.CName SrcLoc.SrcSpanInfo

data Rewrite = Rewrite SrcLoc.SrcSpan Int
             deriving (Show, Eq, Ord)

dummySrcSpanInfo :: SrcLoc.SrcSpanInfo
dummySrcSpanInfo = SrcLoc.SrcSpanInfo (SrcLoc.SrcSpan "" 0 0 0 0) []

dummySrcLoc :: SrcLoc.SrcLoc
dummySrcLoc = SrcLoc.SrcLoc "" 0 0

advanceSrcLoc :: Int -> SrcLoc.SrcLoc -> SrcLoc.SrcLoc
advanceSrcLoc offset (SrcLoc.SrcLoc filename line col) =
  SrcLoc.SrcLoc filename line (col + offset)

buildSrcSpan :: SrcLoc.SrcLoc -> Int -> SrcLoc.SrcSpan
buildSrcSpan (SrcLoc.SrcLoc filename startLine startCol) len =
  SrcLoc.SrcSpan filename startLine startCol startLine (startCol + len)

buildSrcSpanBetween :: SrcLoc.SrcLoc -> SrcLoc.SrcLoc -> SrcLoc.SrcSpan
buildSrcSpanBetween (SrcLoc.SrcLoc name startLine startCol) (SrcLoc.SrcLoc _ endLine endCol)
  = SrcLoc.SrcSpan name startLine startCol endLine endCol

buildSrcSpanInfo :: SrcLoc.SrcLoc -> Int -> SrcLoc.SrcSpanInfo
buildSrcSpanInfo loc len = SrcLoc.SrcSpanInfo (buildSrcSpan loc len) []

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

buildIAbs :: String -> ImportSpec
buildIAbs name =
  Syntax.IAbs dummySrcSpanInfo (Syntax.NoNamespace dummySrcSpanInfo)
    $ buildName name dummySrcLoc

buildIThingAll :: String -> ImportSpec
buildIThingAll name =
  Syntax.IThingAll dummySrcSpanInfo $ buildName name dummySrcLoc

buildVarName :: String -> CName
buildVarName name =
  Syntax.VarName dummySrcSpanInfo $ buildName name dummySrcLoc

buildConName :: String -> CName
buildConName name =
  Syntax.ConName dummySrcSpanInfo $ buildName name dummySrcLoc

buildIThingWith :: String -> [CName] -> ImportSpec
buildIThingWith name =
  Syntax.IThingWith dummySrcSpanInfo (buildName name dummySrcLoc)

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

--------------------------------------------------------------------------------
-- AST SrcLoc marking
--------------------------------------------------------------------------------

class (Syntax.Annotated ast) => Markable ast where
  mark :: ast SrcLoc.SrcSpanInfo -> State SrcLoc.SrcLoc (ast SrcLoc.SrcSpanInfo)

reserveSrcSpan :: String -> State SrcLoc.SrcLoc SrcLoc.SrcSpan
reserveSrcSpan word = do
  startLoc <- get
  let endLoc = advanceSrcLoc (length word) startLoc
  put endLoc
  return $ buildSrcSpanBetween startLoc endLoc

updateSrcInfoSpan
  :: Syntax.Annotated ast
  => State SrcLoc.SrcLoc SrcLoc.SrcSpan
  -> ast SrcLoc.SrcSpanInfo
  -> State SrcLoc.SrcLoc (ast SrcLoc.SrcSpanInfo)
updateSrcInfoSpan spanAction node = do
  span <- spanAction
  return $ Syntax.amap
    (\(SrcLoc.SrcSpanInfo oldSpan oldPoints) ->
      SrcLoc.SrcSpanInfo span oldPoints
    )
    node

appendSrcInfoPoint
  :: Syntax.Annotated ast
  => State SrcLoc.SrcLoc SrcLoc.SrcSpan
  -> ast SrcLoc.SrcSpanInfo
  -> State SrcLoc.SrcLoc (ast SrcLoc.SrcSpanInfo)
appendSrcInfoPoint spanAction node = do
  newPoint <- spanAction
  return $ Syntax.amap
    (\(SrcLoc.SrcSpanInfo span points) ->
      SrcLoc.SrcSpanInfo span (points ++ [newPoint])
    )
    node

importDeclMarkModuleName
  :: Syntax.ImportDecl SrcLoc.SrcSpanInfo
  -> State SrcLoc.SrcLoc (Syntax.ImportDecl SrcLoc.SrcSpanInfo)
importDeclMarkModuleName importDecl = do
  reserveSrcSpan " "
  newModule <- mark $ Syntax.importModule importDecl
  return importDecl { Syntax.importModule = newModule }

importDeclMarkAsModuleName
  :: Syntax.ImportDecl SrcLoc.SrcSpanInfo
  -> State SrcLoc.SrcLoc (Syntax.ImportDecl SrcLoc.SrcSpanInfo)
importDeclMarkAsModuleName importDecl
  | isNothing $ Syntax.importAs importDecl = return importDecl
  | otherwise = do
    reserveSrcSpan " "
    newAs <- mark $ fromJust $ Syntax.importAs importDecl
    return importDecl { Syntax.importAs = Just newAs }

importDeclMarkSpecList
  :: Syntax.ImportDecl SrcLoc.SrcSpanInfo
  -> State SrcLoc.SrcLoc (Syntax.ImportDecl SrcLoc.SrcSpanInfo)
importDeclMarkSpecList importDecl
  | isNothing $ Syntax.importSpecs importDecl = return importDecl
  | otherwise = do
    reserveSrcSpan " "
    newSpecList <- mark $ fromJust $ Syntax.importSpecs importDecl
    return $ importDecl { Syntax.importSpecs = Just newSpecList }

markInner
  :: Markable ast
  => (ast SrcLoc.SrcSpanInfo -> State SrcLoc.SrcLoc (ast SrcLoc.SrcSpanInfo))
  -> ast SrcLoc.SrcSpanInfo
  -> State SrcLoc.SrcLoc (ast SrcLoc.SrcSpanInfo)

markInner inner node = do
  startLoc <- get
  node     <- inner node
  endLoc   <- get
  updateSrcInfoSpan (return $ buildSrcSpanBetween startLoc endLoc) node

markList
  :: Markable ast
  => String
  -> [ast SrcLoc.SrcSpanInfo]
  -> State SrcLoc.SrcLoc ([ast SrcLoc.SrcSpanInfo], [SrcLoc.SrcSpan])
markList _ []     = return ([], [])
markList _ [node] = do
  markedNode <- mark node
  return ([markedNode], [])
markList delim (node : rest) = do
  markedNode               <- mark node
  currDelim                <- reserveSrcSpan delim
  (markedRest, restDelims) <- markList delim rest
  return (markedNode : markedRest, currDelim : restDelims)

conditionalApply :: Bool -> (a -> State s a) -> a -> State s a
conditionalApply True  action node = action node
conditionalApply False _      node = return node

instance Markable Syntax.Name where
  mark node@(Syntax.Ident _ name) =
    updateSrcInfoSpan (reserveSrcSpan name) node

instance Markable Syntax.CName where
  mark node@(Syntax.VarName ann name) = markInner
    (\node -> do
      markedName <- mark name
      return $ Syntax.VarName ann markedName
    )
    node
  mark node@(Syntax.ConName ann name) = markInner
    (\node -> do
      markedName <- mark name
      return $ Syntax.ConName ann markedName
    )
    node

instance Markable Syntax.ImportSpec where
  mark node@(Syntax.IVar ann name) = markInner
    (\node -> do
      markedName <- mark name
      return $ Syntax.IVar ann markedName
    )
    node
  mark node@(Syntax.IAbs ann ns name) = markInner
    (\node -> do
      markedName <- mark name
      return $ Syntax.IAbs ann ns markedName
    )
    node
  mark node@(Syntax.IThingAll ann name) = markInner
    (\node -> do
      markedName <- mark name
      node       <- return $ Syntax.IThingAll ann markedName
      node       <- appendSrcInfoPoint (reserveSrcSpan "(") node
      node       <- appendSrcInfoPoint (reserveSrcSpan "..") node
      appendSrcInfoPoint (reserveSrcSpan ")") node
    )
    node
  mark node@(Syntax.IThingWith (SrcLoc.SrcSpanInfo span points) name withs) =
    markInner
      (\node -> do
        markedName                <- mark name
        startSpan                 <- reserveSrcSpan "("
        (markedWiths, delimSpans) <- markList ", " withs
        endSpan                   <- reserveSrcSpan ")"
        return $ Syntax.IThingWith
          (SrcLoc.SrcSpanInfo span ([startSpan] ++ delimSpans ++ [endSpan]))
          markedName
          markedWiths
      )
      node

instance Markable Syntax.ImportSpecList where
  mark = markInner
    (\node@(Syntax.ImportSpecList (SrcLoc.SrcSpanInfo span points) hiding specs) ->
      do
        startSpan                 <- reserveSrcSpan "("
        (markedSpecs, delimSpans) <- markList ", " specs
        endSpan                   <- reserveSrcSpan ")"
        return $ Syntax.ImportSpecList
          (SrcLoc.SrcSpanInfo span ([startSpan] ++ delimSpans ++ [endSpan]))
          hiding
          markedSpecs
    )

instance Markable Syntax.ModuleName where
  mark node@(Syntax.ModuleName _ name) =
    updateSrcInfoSpan (reserveSrcSpan name) node

instance Markable Syntax.ImportDecl where
  mark node@(Syntax.ImportDecl _ modName qual _ _ _ maybeAs maybeSpecs) =
    markInner
      (\node -> do
        node <- appendSrcInfoPoint (reserveSrcSpan "import") node
        node <- conditionalApply
          qual
          (\node -> do
            reserveSrcSpan " "
            appendSrcInfoPoint (reserveSrcSpan "qualified") node
          )
          node
        node <- importDeclMarkModuleName node
        node <- conditionalApply
          (isJust maybeAs)
          (\node -> do
            reserveSrcSpan " "
            appendSrcInfoPoint (reserveSrcSpan "as") node
          )
          node
        node <- importDeclMarkAsModuleName node
        importDeclMarkSpecList node
      )
      node

markImportDecl
  :: Syntax.ImportDecl SrcLoc.SrcSpanInfo
  -> SrcLoc.SrcLoc
  -> Syntax.ImportDecl SrcLoc.SrcSpanInfo
markImportDecl importDecl = evalState (mark importDecl)
