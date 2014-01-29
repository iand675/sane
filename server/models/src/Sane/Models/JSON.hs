module Sane.Models.JSON where
import Data.Aeson.TH
import Data.Char
import Data.List
import Prelude
import Language.Haskell.TH

makePretty [] = []
makePretty (x:xs) = toLower x : xs

jsonSettings = defaultOptions
  { fieldLabelModifier = makePretty . dropWhile (not . isUpper)
  , constructorTagModifier = makePretty
  , omitNothingFields = True
  , allNullaryToStringTag = True
  , sumEncoding = TaggedObject { tagFieldName = "type", contentsFieldName = "value" }
  }

jsonize n = deriveJSON settings n
  where
    settings = jsonSettings
      { constructorTagModifier = \s -> makePretty $ take (length s - (length $ nameBase n)) s
      }
