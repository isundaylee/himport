module HImport.Util
  ( isIdentQualified
  , importedName
  , importEntry
  , splitTokens
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

isIdentQualified :: String -> Bool
isIdentQualified =
  liftA2 (&&) (not . null . filter (== '.')) (not . null . filter (/= '.'))

splitTokens :: String -> [String]
splitTokens = map unpack . splitOn "." . pack

hasMarker :: String -> Bool
hasMarker = (== '\'') . last

removeMarker :: String -> String
removeMarker str = if hasMarker str then init str else str

importedName :: String -> String
importedName ident = if (null $ filter (== '\'') ident)
  then ident
  else
    let tokens = splitTokens ident
    in  intercalate "." $ map removeMarker $ dropWhile (not <$> hasMarker)
                                                       tokens

moduleName :: String -> Maybe String
moduleName ident = if (null moduleTokens)
  then Nothing
  else Just $ intercalate "." moduleTokens
 where
  moduleTokens =
    reverse $ drop 1 $ reverse $ map removeMarker $ splitTokens ident

objectName :: String -> String
objectName = removeMarker . last . splitTokens

importEntry :: String -> (String, String, Maybe String)
importEntry ident =
  let modu   = fromJust $ moduleName ident
      object = objectName ident
      name   = importedName ident
  in  (modu, object, moduleName name)
