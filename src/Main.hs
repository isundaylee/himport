import           System.Environment             ( getArgs )

import           HaskellAutoImporter.AutoImporter
                                                ( autoImport )

main :: IO ()
main = interact $ autoImport
