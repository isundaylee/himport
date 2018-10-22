module HaskellAutoImporter.Util
  ( isIdentQualified
  )
where

isIdentQualified :: String -> Bool
isIdentQualified = not . null . filter (== '.')
