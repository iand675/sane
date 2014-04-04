{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Sane.Wire.Tasks where
import           Data.Word
import           Data.ProtocolBuffers
import           Data.Text
import           Data.TypeLevel
import           GHC.Generics (Generic)
import           Sane.Wire.Common

data Task = Task
  { taskName      :: Required D1 (Value Text)
  , taskCreatorId :: Required D2 (Value Id)
  , taskListId    :: Optional D3 (Value Id)
  } deriving (Generic, Show)

instance Encode Task
instance Decode Task

type PersistedTask = Persisted Task
