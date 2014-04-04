{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Sane.Wire.Lists where
import           Data.ProtocolBuffers
import           Data.Int
import           Data.Text
import           Data.TypeLevel hiding (Bool)
import           GHC.Generics (Generic)
import           Sane.Wire.Common

data List = List
  { listName     :: Required D1 (Value Text)
  , listIcon     :: Optional D2 (Value Text)
  , listArchived :: Required D3 (Value Bool)
  , listOwner    :: Required D4 (Value Int64)
  } deriving (Generic, Show)

instance Encode List
instance Decode List

type PersistedList = Persisted List
