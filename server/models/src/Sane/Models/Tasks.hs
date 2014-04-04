{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sane.Models.Tasks where
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import           Network.HTTP.JSON.API
import Prelude hiding (id)

import Sane.Models.Common
import Sane.Models.JSON

data TaskState
  = Incomplete
  | Accomplished
  | Overdue
  deriving (Eq, Show)

data Task = Task
  { _taskTitle        :: Text
  , _taskDescription  :: Maybe Text
  , _taskStatus       :: TaskState
  , _taskAssignedUser :: Maybe Text
  , _taskState        :: TaskState
  , _taskDeadline     :: Maybe UTCTime
  , _taskCreator      :: Text
  , _taskArchived     :: Bool
  } deriving (Eq, Show)

{-
  private:
    archived
    list
    reminders
-}

data TaskEvent = Comment | Created | Updated | Deleted
  deriving (Eq, Show)

makeFields ''Task
jsonize ''TaskState
jsonize ''TaskEvent
jsonize ''Task

instance JsonAPIDocument (Persisted Task) where
  documentType = to $ const "task"
