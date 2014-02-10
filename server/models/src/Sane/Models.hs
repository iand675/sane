{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Sane.Models where
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson (ToJSON, FromJSON, object, (.=), toJSON, Value(..), Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64 (encode, decodeLenient)
import Data.HashMap.Strict (singleton)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Facebook (UserAccessToken)

import Sane.Models.JSON

data Session = Session
  { _sessionToken :: ByteString
  , _sessionExpiration :: UTCTime
  }

makeFields ''Session

session tok time = Session (B64.encode tok) time
fromSession s = s ^. token . to B64.decodeLenient

data CreateUserError = UsernameExists | StripeError

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

data NewUser
  = StandardNewUser
    { _nuUsername  :: Text
    , _nuEmail     :: Text
    , _nuName      :: Text
    , _nuPassword  :: Text
    }
  | FacebookNewUser
    { _nuAccessToken    :: Text
    , _nuUserId         :: Text
    , _nuExpirationTime :: Integer
    }

makeFields ''NewUser

data FullUser = FullUser
  { _fuUsername      :: Username
  , _fuName          :: Text
  , _fuEmail         :: Text
  , _fuCellphone     :: Maybe Text
  , _fuAvatar        :: Maybe Text
  , _fuStripeToken   :: Maybe Text
  , _fuFacebookAuth  :: Maybe UserAccessToken
  } deriving (Eq, Show)

type instance Id FullUser = Int

makeFields ''FullUser

data User = User
  { _uUsername :: Username
  , _uName     :: Text
  , _uAvatar   :: Maybe Text
  } deriving (Eq, Show)

type instance Id User = Int

makeFields ''User

data CurrentUser = CurrentUser
  { _cuUsername  :: Text
  , _cuEmail     :: Text
  , _cuName      :: Text
  , _cuCellphone :: Maybe Text
  , _cuAvatar    :: Maybe Text
  }

makeFields ''CurrentUser

data SignIn
  = StandardSignIn
    { _siUsername :: Username
    , _siPassword :: Password
    }
  | FacebookSignIn
    { _siAccessToken :: Text
    , _siUserId      :: Text
    , _siExpiration  :: UTCTime
    }

makeFields ''SignIn

data Membership = Membership
  { _membershipKind     :: MembershipKind
  , _membershipUsername :: Username
  } deriving (Eq, Show)

makeFields ''Membership

data List = List
  { _listTitle   :: Text
  , _listIcon    :: Text
  } deriving (Eq, Show)

type instance Id List = Int

makeFields ''List

data Task = Task
  { _taskTitle       :: Text
  , _taskComplete    :: Bool
  , _taskDeadline    :: Maybe UTCTime
  , _taskDescription :: Maybe Text
  -- , _taskAssignedTo  :: Maybe Username
  -- , _taskReminders   :: [Reminder]
  } deriving (Eq, Show)

data ReminderWindow = TimesOfDay
  { _reminderWindowMorning   :: Bool
  , _reminderWindowAfternoon :: Bool
  , _reminderWindowEvening   :: Bool
  } deriving (Eq, Show)

data Channel
  = EmailChannel
  | SMS
  | FacebookChannel
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

jsonize ''NewUser
jsonize ''CurrentUser
jsonize ''SignIn
jsonize ''List

data Result a
  = Result a
  | Error { _resultMessage :: Text, _resultData :: Value }

instance ToJSON a => ToJSON (Result a) where
  toJSON (Result x) = toJSON x
  toJSON (Error m r) = object [ "message" .= m, "data" .= r ]

instance (ToJSON a, ToJSON (Id a)) => ToJSON (Persisted a) where
  toJSON (Persisted pid pval) = case toJSON pval of
    (Object o) -> Object (singleton "id" (toJSON pid) <> o)
    j -> object [ "id" .= pid, "value" .= j ]

result :: a -> Result a
result = Result

errorResult :: ToJSON a => Text -> a -> Result b
errorResult m d = Error m $ toJSON d
