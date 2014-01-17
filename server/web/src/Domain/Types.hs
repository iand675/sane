{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Domain.Types where
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

type family Id a
type family Patch a
type Username = Text

data Persisted a = Persisted
  { persistedId    :: Id a
  , persistedValue :: a
  }

data MembershipKind = Owner | Member
  deriving (Eq, Show)

data ListIcon
  = Home
  | Work
  | Shopping
  deriving (Eq, Show)

data FullUser = FullUser
  { userUsername      :: Username
  , userName          :: Text
  , userEmail         :: Text
  , userCellphone     :: Maybe Text
  , userAvatar        :: Maybe Text
  , userStripeToken   :: Maybe Text
  , userFacebookToken :: Maybe Text
  , userPersonaToken  :: Maybe Text
  } deriving (Eq, Show)

data Membership = Membership
  { membershipKind     :: MembershipKind
  , membershipUsername :: Username
  } deriving (Eq, Show)

data List = List
  { listTitle   :: Text
  , listIcon    :: Maybe ListIcon
  , listMembers :: [Membership]
  } deriving (Eq, Show)

data Task = Task
  { taskTitle       :: Text
  , taskComplete    :: Bool
  , taskDeadline    :: Maybe UTCTime
  , taskDescription :: Maybe Text
  , taskAssignedTo  :: Username
  , taskReminders   :: [Reminder]
  } deriving (Eq, Show)

data ReminderWindow = TimesOfDay
  { reminderWindowMorning   :: Bool
  , reminderWindowAfternoon :: Bool
  , reminderWindowEvening   :: Bool
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

