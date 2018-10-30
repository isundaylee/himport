module HImport.ASTUtilSpec
  ( spec
  )
where
import           Text.Show.Prettyprint          ( prettyShow )
import           Debug.Trace                    ( trace )
import           Data.List                      ( intercalate )
import qualified Language.Haskell.Exts.ExactPrint
                                               as ExactPrint
                                                ( exactPrint )
import qualified Language.Haskell.Exts.Parser  as Parser
                                                ( defaultParseMode
                                                , ParseResult(..)
                                                , parseModuleWithMode
                                                )
import qualified Language.Haskell.Exts.SrcLoc  as SrcLoc
                                                ( SrcSpanInfo(..)
                                                , SrcSpan(..)
                                                , SrcLoc(..)
                                                )
import qualified Language.Haskell.Exts.Syntax  as Syntax
                                                ( Module(..) )
import           Test.Hspec
import           HImport.ASTUtil

parse :: String -> Syntax.Module SrcLoc.SrcSpanInfo
parse code =
  let parseMode = Parser.defaultParseMode
      result    = Parser.parseModuleWithMode parseMode code
  in  case result of
        Parser.ParseOk tree -> tree

spec :: Spec
spec = do
  describe "applyRewrite" $ do
    it "should work for single-line case" $ do
      let newTree =
            (applyRewrite (parse "main = intercalate           test")
                          (Rewrite (SrcLoc.SrcSpan "" 1 8 1 29) (-10))
            )
      (ExactPrint.exactPrint newTree []) `shouldBe` "main = intercalate test"

    it "should work for multi-line case" $ do
      let newTree =
            (applyRewrite
              (parse $ intercalate
                "\n"
                ["main = intercalate           test", "another = nothing"]
              )
              (Rewrite (SrcLoc.SrcSpan "" 1 8 1 29) (-10))
            )
      (ExactPrint.exactPrint newTree [])
        `shouldBe` (intercalate
                     "\n"
                     ["main = intercalate test", "another = nothing"]
                   )

  describe "applyRewrites" $ do
    it "should work for multiple rewrites on the same line" $ do
      let newTree =
            (applyRewrites
              (parse "main = abc    def    other")
              [ Rewrite (SrcLoc.SrcSpan "" 1 8 1 14)  (-3)
              , Rewrite (SrcLoc.SrcSpan "" 1 15 1 21) (-3)
              ]
            )
          newTreeReverseOrder =
            (applyRewrites
              (parse "main = abc    def    other")
              [ Rewrite (SrcLoc.SrcSpan "" 1 15 1 21) (-3)
              , Rewrite (SrcLoc.SrcSpan "" 1 8 1 14)  (-3)
              ]
            )
      (ExactPrint.exactPrint newTree []) `shouldBe` "main = abc def other"
      (ExactPrint.exactPrint newTreeReverseOrder [])
        `shouldBe` "main = abc def other"

  describe "markImportDecl" $ do
    it "should work with a basic unqualified import statement" $ do
      let importDecl =
            buildUnqualifiedImportDecl "Test.Module" $ buildImportSpecList $ map
              buildIVar
              ["a", "b", "c"]
      (ExactPrint.exactPrint
          (markImportDecl importDecl $ SrcLoc.SrcLoc "" 1 1)
          []
        )
        `shouldBe` "import Test.Module (a, b, c)"

    it "should work with a basic qualified import statement" $ do
      let importDecl =
            buildQualifiedImportDecl "Test.Module" "Module"
              $ buildImportSpecList
              $ map buildIVar ["a", "b", "c"]
      (ExactPrint.exactPrint
          (markImportDecl importDecl $ SrcLoc.SrcLoc "" 1 1)
          []
        )
        `shouldBe` "import qualified Test.Module as Module (a, b, c)"

    it "should work with IAbs" $ do
      let importDecl =
            buildQualifiedImportDecl "Test.Module" "Module"
              $ buildImportSpecList
              $ map buildIAbs ["A", "B", "C"]
      (ExactPrint.exactPrint
          (markImportDecl importDecl $ SrcLoc.SrcLoc "" 1 1)
          []
        )
        `shouldBe` "import qualified Test.Module as Module (A, B, C)"

    it "should work with IThingAll" $ do
      let importDecl =
            buildQualifiedImportDecl "Test.Module" "Module"
              $ buildImportSpecList
              $ map buildIThingAll ["A", "B", "C"]
      (ExactPrint.exactPrint
          (markImportDecl importDecl $ SrcLoc.SrcLoc "" 1 1)
          []
        )
        `shouldBe` "import qualified Test.Module as Module (A(..), B(..), C(..))"

    it "should work with IThingWith" $ do
      let importDecl =
            buildQualifiedImportDecl "Test.Module" "Module"
              $ buildImportSpecList
                  [buildIThingWith "C" [buildVarName "xxx", buildConName "Yyy"]]

      (ExactPrint.exactPrint
          (markImportDecl importDecl $ SrcLoc.SrcLoc "" 1 1)
          []
        )
        `shouldBe` "import qualified Test.Module as Module (C(xxx, Yyy))"
