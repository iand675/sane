{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Sane.Wire.Common (
  Id,
  Persisted(..)
) where
import           Data.Monoid
import           Data.ProtocolBuffers
import           Data.ProtocolBuffers.Internal
import           Data.TypeLevel
import           Data.Int
import           GHC.Generics (Generic)

type Id = Int64

data Persisted v = Persisted
  { persistedId    :: Required D1 (Value Id)
  , persistedValue :: Required D2 (Message v)
  } deriving (Generic)

instance (Encode v) => Encode (Persisted v)
instance (Decode v, Monoid (Message v)) => Decode (Persisted v)
