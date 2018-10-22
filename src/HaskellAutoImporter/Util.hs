module HaskellAutoImporter.Util
  ( isIdentQualified
  , importedName
  )
where

import           Data.List                      ( findIndex )

isIdentQualified :: String -> Bool
isIdentQualified = not . null . filter (== '.')

importedName :: String -> String
importedName ident = case (findIndex (== '@') ident) of
  Nothing -> ident
  Just i  -> (drop (i + 1) ident)
