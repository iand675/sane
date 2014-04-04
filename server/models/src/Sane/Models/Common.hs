{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Sane.Models.Common where
import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson (ToJSON, FromJSON, object, (.=), toJSON, Value(..), Object)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (singleton)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.ProtocolBuffers.Lens
import Data.Text (Text)
import Data.Word (Word64)
import Prelude (Int, ($), (.), Show, Eq, fromIntegral, undefined)

import Network.HTTP.JSON.API
import Sane.Models.JSON
import Sane.Routes hiding (Id(..))
import qualified Sane.Wire.Common as W

newtype Id a = Id { fromId :: Int }
  deriving (ToJSON, FromJSON, Show, Eq)

class ToModel domain model | model -> domain where
  toModel :: Getter domain model

class FromModel domain model | model -> domain where
  fromModel :: Getter model domain

data Result a
  = Result (Root a)
  | Error { _resultMessage :: Text, _resultData :: Value }

instance ToJSON a => ToJSON (Result a) where
  toJSON (Result x) = toJSON x
  toJSON (Error m r) = object [ "message" .= m, "data" .= r ]

data Persisted a = Persisted
  { _persistedId    :: Id a
  , _persistedValue :: a
  }

makeFields ''Persisted

instance (ProtocolBuffer a a') => ProtocolBuffer (Persisted a) (W.Persisted a') where
  proto = iso toWire fromWire
    where
      toWire = W.Persisted
        <$> view (id . to (fromIntegral . fromId) . field)
        <*> view (value . proto . field)
      {-
        <$> view (id . to (fromIntegral . fromId) . field)
        <*> view (value . field)
        -}
      fromWire = Persisted
        <$> ((view $ from field . to (Id . fromIntegral)) . W.persistedId)
        <*> ((view $ from field . from proto) . W.persistedValue)

persisted :: (ToJSON a, JsonAPIDocument a) => Prism' (Document a) (Persisted a)
persisted = prism' toDocument fromDocument
  where
    toDocument p = (doc $ p ^. value)
      & documentId . _Just . intId .~ p ^. id . to fromId
    fromDocument d = Persisted
      <$> (d ^? documentId . _Just . intId . to Id)
      <*> (d ^? documentValue)

class Routable a where
  routed :: SaneAction -> a -> a

instance Routable (Document a) where
  routed r = set documentHref (saneRoute r ^? _Just . _2)

instance Routable LinkValue where
  routed r = set linkHref (saneRoute r ^? _Just . _2)

instance (ToJSON a) => ToJSON (Persisted a) where
  toJSON (Persisted pid pval) = case toJSON pval of
    (Object o) -> Object (singleton "id" (toJSON pid) <> o)
    j -> object [ "id" .= pid, "value" .= j ]

result :: Root a -> Result a
result = Result

errorResult :: ToJSON a => Text -> a -> Result b
errorResult m d = Error m $ toJSON d

data Collection a = Collection { _collectionItems :: [a] }

jsonize ''Collection
