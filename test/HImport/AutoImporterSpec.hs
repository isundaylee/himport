module HImport.AutoImporterSpec
  ( spec
  )
where

import           Test.Hspec
import           HImport.AutoImporter

import           Data.List                      ( intercalate )

joinLines :: [String] -> String
joinLines = intercalate "\n"

spec :: Spec
spec = do
  describe "autoImport" $ do
    it "return the original code if not able to parse" $ do
      (autoImport "``") `shouldBe` "``"

    it "should add missing import (implicitly fully qualified)" $ do
      (autoImport (joinLines ["main = Data.List.something"]))
        `shouldBe` (joinLines
                     [ "import qualified Data.List as Data.List (something)"
                     , "main = Data.List.something"
                     ]
                   )

    it "should add missing import (explicitly fully qualified)" $ do
      (autoImport (joinLines ["main = Data'.List.something"]))
        `shouldBe` (joinLines
                     [ "import qualified Data.List as Data.List (something)"
                     , "main = Data.List.something"
                     ]
                   )

    it "should add missing import (partially qualified)" $ do
      (autoImport (joinLines ["main = Data.List'.something"]))
        `shouldBe` (joinLines
                     [ "import qualified Data.List as List (something)"
                     , "main = List.something"
                     ]
                   )

    it "should add missing import (unqualified)" $ do
      (autoImport (joinLines ["main = Data.List.something'"]))
        `shouldBe` (joinLines
                     ["import Data.List (something)", "main = something"]
                   )

    it "should not add duplicate import (implicitly fully qualified)" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as Data.List (something)"
            , "main = Data.List.something"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as Data.List (something)"
                     , "main = Data.List.something"
                     ]
                   )

    it "should not add duplicate import (explicitly fully qualified)" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as Data.List (something)"
            , "main = Data'.List.something"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as Data.List (something)"
                     , "main = Data.List.something"
                     ]
                   )

    it "should not add duplicate import (partially qualified)" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as List (something)"
            , "main = Data.List'.something"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as List (something)"
                     , "main = List.something"
                     ]
                   )

    it "should not add duplicate import (unqualified)" $ do
      (autoImport
          (joinLines
            ["import Data.List (something)", "main = Data.List.something'"]
          )
        )
        `shouldBe` (joinLines
                     ["import Data.List (something)", "main = something"]
                   )

    it "should add to existing import clauses (implicitly fully qualified)" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as Data.List (something)"
            , "main = Data.List.somethingElse"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as Data.List (something, somethingElse)"
                     , "main = Data.List.somethingElse"
                     ]
                   )

    it "should add to existing import clauses (explicitly fully qualified)" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as Data.List (something)"
            , "main = Data'.List.somethingElse"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as Data.List (something, somethingElse)"
                     , "main = Data.List.somethingElse"
                     ]
                   )

    it "should add to existing import clauses (partially qualified)" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as List (something)"
            , "main = Data.List'.somethingElse"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as List (something, somethingElse)"
                     , "main = List.somethingElse"
                     ]
                   )

    it "should add to existing import clauses (unqualified)" $ do
      (autoImport
          (joinLines
            ["import Data.List (something)", "main = Data.List.somethingElse'"]
          )
        )
        `shouldBe` (joinLines
                     [ "import Data.List (something, somethingElse)"
                     , "main = somethingElse"
                     ]
                   )

    it "should recognize already imported partial imports" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as List (something)"
            , "main = List.something"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as List (something)"
                     , "main = List.something"
                     ]
                   )

    it "should insert into already imported partial imports" $ do
      (autoImport
          (joinLines
            [ "import qualified Data.List as List (something)"
            , "main = List.somethingElse"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import qualified Data.List as List (something, somethingElse)"
                     , "main = List.somethingElse"
                     ]
                   )
