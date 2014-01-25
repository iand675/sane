module Web.JSON where
import Data.Aeson.TH
import Data.Char
import Data.List
import Prelude

makePretty [] = []
makePretty (x:xs) = toLower x : xs

jsonSettings = defaultOptions { fieldLabelModifier = makePretty . dropWhile (not . isUpper) }