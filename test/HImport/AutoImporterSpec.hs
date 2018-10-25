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

    it "should import all constructors for a data type" $ do
      (autoImport
          (joinLines
            [ "main :: Data.List.List' -> String"
            , "main list = Data.List.toString' list"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import Data.List (toString, List(..))"
                     , ""
                     , "main :: List -> String"
                     , "main list = toString list"
                     ]
                   )

    it "should not import duplicate type" $ do
      (autoImport
          (joinLines
            [ "import Data.List (toString, List(..))"
            , "main :: Data.List.List' -> String"
            , "main list = Data.List.toString' list"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import Data.List (toString, List(..))"
                     , ""
                     , "main :: List -> String"
                     , "main list = toString list"
                     ]
                   )

    it "should not import constructors separately" $ do
      (autoImport
          (joinLines
            [ "import Data.List (List(..))"
            , ""
            , "main :: Data.List.List'"
            , "main = Data.List.Empty'"
            ]
          )
        )
        `shouldBe` (joinLines
                     [ "import Data.List (List(..))"
                     , ""
                     , "main :: List"
                     , "main = Empty"
                     ]
                   )

    it "should not collapse different unqualified imports" $ do
      (autoImport
          (joinLines
            ["import Data.List (something)", "main = Data.Maybe.somethingElse'"]
          )
        )
        `shouldBe` (joinLines
                     [ "import Data.Maybe (somethingElse)"
                     , "import Data.List (something)"
                     , "main = somethingElse"
                     ]
                   )

    it "should collapse matching unqualified imports" $ do
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
