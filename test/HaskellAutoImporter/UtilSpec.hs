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

  describe "importedName" $ do
    it "should work in cases with @" $ do
      (importedName "Data.List.@map") `shouldBe` "map"
      (importedName "Data.@List.map") `shouldBe` "List.map"

    it "should work in cases without @" $ do
      (importedName "Data.List.map") `shouldBe` "Data.List.map"
