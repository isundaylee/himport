module HImport.Util
  ( isIdentQualified
  , importedName
  , importVarEntry
  , importTypeEntry
  , splitTokens
  , ImportEntry
  , ImportObject(..)
  )
where

import           Control.Applicative            ( liftA2 )

import           Data.List                      ( findIndex
                                                , intercalate
                                                )
import           Data.Sequence                  ( findIndexR )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( splitOn
                                                , pack
                                                , unpack
                                                )

data ImportObject = ImportVar String | ImportType String deriving (Show, Eq);

type ImportEntry = (String, ImportObject, Maybe String)

isIdentQualified :: String -> Bool
isIdentQualified = liftA2 (&&) ('.' `elem`) (any (/= '.'))

splitTokens :: String -> [String]
splitTokens = map unpack . splitOn "." . pack

hasMarker :: String -> Bool
hasMarker = (== '\'') . last

removeMarker :: String -> String
removeMarker str = if hasMarker str then init str else str

importedName :: String -> String
importedName ident = if '\'' `notElem` ident
  then ident
  else
    let tokens = splitTokens ident
    in  intercalate "." $ map removeMarker $ dropWhile (not <$> hasMarker)
                                                       tokens

moduleName :: String -> Maybe String
moduleName ident = if null moduleTokens
  then Nothing
  else Just $ intercalate "." moduleTokens
 where
  moduleTokens =
    reverse $ drop 1 $ reverse $ map removeMarker $ splitTokens ident

objectName :: String -> String
objectName = removeMarker . last . splitTokens

importVarEntry :: String -> ImportEntry
importVarEntry ident =
  ( fromJust $ moduleName ident
  , ImportVar $ objectName ident
  , moduleName $ importedName ident
  )

importTypeEntry :: String -> ImportEntry
importTypeEntry ident =
  ( fromJust $ moduleName ident
  , ImportType $ objectName ident
  , moduleName $ importedName ident
  )
