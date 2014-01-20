{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Domain.Types where
import Control.Lens.TH
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Common

type family Id a
type family Patch a
type Username = Text
type Password = Text

data Persisted a = Persisted
  { _persistedId    :: Id a
  , _persistedValue :: a
  }

data MembershipKind = Owner | Member
  deriving (Eq, Show)

data NewUser = NewUser
  { _nuUsername  :: Text
  , _nuEmail     :: Text
  , _nuName      :: Text
  , _nuPassword  :: Text
  }

makeFields ''NewUser

data FullUser = FullUser
  { _fuUsername      :: Username
  , _fuName          :: Text
  , _fuEmail         :: Text
  , _fuCellphone     :: Maybe Text
  , _fuAvatar        :: Maybe Text
  , _fuStripeToken   :: Maybe Text
  , _fuFacebookToken :: Maybe Text
  } deriving (Eq, Show)

makeFields ''FullUser

data User = User
  { _uUsername :: Username
  , _uName     :: Text
  , _uAvatar   :: Maybe Text
  } deriving (Eq, Show)

makeFields ''User

data CurrentUser = CurrentUser
  { _cuUsername  :: Text
  , _cuEmail     :: Text
  , _cuName      :: Text
  , _cuCellphone :: Maybe Text
  , _cuAvatar    :: Maybe Text
  }

makeFields ''CurrentUser

data Membership = Membership
  { _membershipKind     :: MembershipKind
  , _membershipUsername :: Username
  } deriving (Eq, Show)

makeFields ''Membership

data List = List
  { _listTitle   :: Text
  , _listMembers :: [Membership]
  } deriving (Eq, Show)

makeFields ''List

data Task = Task
  { _taskTitle       :: Text
  , _taskComplete    :: Bool
  , _taskDeadline    :: Maybe UTCTime
  , _taskDescription :: Maybe Text
  , _taskAssignedTo  :: Username
  , _taskReminders   :: [Reminder]
  } deriving (Eq, Show)

data ReminderWindow = TimesOfDay
  { _reminderWindowMorning   :: Bool
  , _reminderWindowAfternoon :: Bool
  , _reminderWindowEvening   :: Bool
  } deriving (Eq, Show)

data Channel
  = Email
  | SMS
  | Facebook
  deriving (Eq, Show)

data Reminder = Reminder
  { reminderChannel     :: Channel
  , reminderWindow      :: ReminderWindow
  , reminderTimesPerDay :: Int
  } deriving (Eq, Show)

class FromData domain dat | domain -> dat where
  dbItem :: Iso' domain dat

class FromModel domain model | model -> domain where
  model :: Iso' domain model
