module HaskellAutoImporter.UtilSpec
  ( spec
  )
where

import           Test.Hspec
import           HaskellAutoImporter.Util

spec :: Spec
spec = do
  describe "isIdentQualified" $ do
    it "return true with qualified ident" $ do
      (isIdentQualified "Data.List.map") `shouldBe` True
    it "return false with unqualified ident" $ do
      (isIdentQualified "map") `shouldBe` False
