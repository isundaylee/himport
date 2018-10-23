import           System.Environment             ( getArgs )

import           HImport.AutoImporter
                                                ( autoImport )

main :: IO ()
main = interact $ autoImport
