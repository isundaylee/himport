module HImport.UtilSpec
  ( spec
  )
where

import           Test.Hspec
import           HImport.Util

spec :: Spec
spec = do
  describe "isIdentQualified" $ do
    it "return true with qualified ident" $ do
      (isIdentQualified "Data.List.map") `shouldBe` True

    it "return false with unqualified ident" $ do
      (isIdentQualified "map") `shouldBe` False

    it "return false with only dots" $ do
      (isIdentQualified ".") `shouldBe` False
      (isIdentQualified "..") `shouldBe` False

  describe "importedName" $ do
    it "should work in cases with '" $ do
      (importedName "Data.List.map'") `shouldBe` "map"
      (importedName "Data.List'.map") `shouldBe` "List.map"
      (importedName "Data'.List'.map") `shouldBe` "Data.List.map"

    it "should work in cases without '" $ do
      (importedName "Data.List.map") `shouldBe` "Data.List.map"

  describe "importEntry" $ do
    it "should work with qualified import" $ do
      (importEntry "Data.List'.map")
        `shouldBe` ("Data.List", "map", Just "List")
      (importEntry "Data.List.map")
        `shouldBe` ("Data.List", "map", Just "Data.List")

    it "should work with unqualified import" $ do
      (importEntry "Data.List.map'") `shouldBe` ("Data.List", "map", Nothing)
