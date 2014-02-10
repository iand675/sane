{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Sane.Web.Miscellaneous where
import Data.Aeson
import Data.ByteString.Char8 (tail, span)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.List as L
import Data.Time
import Network.HTTP.Types
import Network.Webmachine
import Web.Cookie

import Sane.Common
import Sane.Models
import Sane.Models.JSON

data Pong = Pong { pongPong :: Bool }

jsonize ''Pong

api :: (ToJSON responseBody, FromJSON body) => (Maybe body -> Webmachine c s responseBody) -> Resource c s a body responseBody
api = supportJSON . basic

ping :: Resource c s a () Pong
ping = api $ \_ -> return $ Pong True

getSaneCookie :: Webmachine c s (Maybe (Username, Session))
getSaneCookie = do
  c <- getHeader hCookie
  case c of
    Nothing -> return Nothing
    Just bs -> do
      let cookies = parseCookies bs
      case L.lookup "SANE" cookies of
        Nothing -> return Nothing
        Just saneCookie -> do
          let (u, s) = span (/= ':') saneCookie
          let s' = tail s
          return $ Just (decodeUtf8 u, Session s' $ UTCTime (ModifiedJulianDay 0) 0)
