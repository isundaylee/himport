module HImport.Util
  ( isIdentQualified
  , importedName
  , importEntry
  , splitTokens
  , ImportEntry
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

type ImportEntry = (String, String, Maybe String)

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

importEntry :: String -> ImportEntry
importEntry ident =
  let modu   = fromJust $ moduleName ident
      object = objectName ident
      name   = importedName ident
  in  (modu, object, moduleName name)
