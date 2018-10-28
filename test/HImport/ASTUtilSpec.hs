module HImport.ASTUtilSpec
  ( spec
  )
where
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
