module HaskellAutoImporter.Util
  ( isIdentQualified
  , importedName
  , importEntry
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

importedName :: String -> String
importedName ident = case (findIndex (== '@') ident) of
  Nothing -> ident
  Just i  -> (drop (i + 1) ident)

moduleName :: String -> Maybe String
moduleName ident = if (null packageTokens)
  then Nothing
  else Just $ intercalate "." packageTokens
 where
  tokens        = map (filter (/= '@')) $ map unpack (splitOn "." $ pack ident)
  packageTokens = reverse (drop 1 (reverse tokens))

objectName :: String -> String
objectName = filter (/= '@') . unpack . last . splitOn "." . pack

importEntry :: String -> (String, String, Maybe String)
importEntry ident =
  let package = fromJust $ moduleName ident
      object  = objectName ident
      name    = importedName ident
  in  (package, object, moduleName name)
