{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Types where
import Control.Lens.Iso
import Control.Lens.TH
import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField hiding (name)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Prelude (Enum(..))
import Common

import qualified Domain.Types as Domain

type family Id a
type instance Id List = UUID

data List = List
  { listTitle :: Text
  } deriving (Eq, Show)

instance Domain.FromData Domain.List List where
  dbItem = iso toData fromData
    where
      toData l = List
        { listTitle = Domain._listTitle l
        }
      fromData l = Domain.List
        { Domain._listTitle = listTitle l
        , Domain._listMembers = []
        }

type instance Id Task = UUID

data Task = Task
  { taskTitle       :: Text
  , taskDeadline    :: Maybe UTCTime
  , taskDescription :: Maybe Text
  , taskAssignedTo  :: Maybe (Id User)
  } deriving (Eq, Show)

type instance Id User = Int

data MembershipKind
  = Owner
  | Member
  deriving (Eq, Show, Enum, Typeable)

instance Hashable MembershipKind where
  hashWithSalt = hashUsing fromEnum

rawMembershipKinds :: H.HashMap ByteString MembershipKind
rawMembershipKinds = H.fromList [("owner", Owner), ("member", Member)]

membershipKinds :: H.HashMap MembershipKind ByteString
membershipKinds = H.fromList [(Owner, "owner"), (Member, "member")]

instance FromField MembershipKind where
  fromField field bs = do
    name <- fromField field bs
    case rawMembershipKinds ^. at name of
      Nothing -> returnError ConversionFailed field "Invalid MembershipKind value"
      Just k -> return k

data User = User
  { _userUsername      :: Text
  , _userName          :: Text
  , _userEmail         :: Text
  , _userPasswordHash  :: ByteString
  , _userCellphone     :: Maybe Text
  , _userAvatar        :: Maybe Text
  , _userStripeToken   :: Maybe Text
  , _userFacebookToken :: Maybe Text
  } deriving (Eq, Show)

makeFields ''User

data Membership = Membership
  { membershipListId :: Id List
  , membershipUserId :: Id User
  , membershipKind   :: MembershipKind
  } deriving (Eq, Show)

data ListTasks = ListTasks
  { listTasksTaskId :: Id Task
  , listTasksListId :: Id List
  } deriving (Eq, Show)

fullUser :: Getter User Domain.FullUser
fullUser = to $ \u -> Domain.FullUser
  { Domain._fuUsername = u ^. username
  , Domain._fuName = u ^. email
  , Domain._fuCellphone = u ^. cellphone
  , Domain._fuAvatar = u ^. avatar
  , Domain._fuStripeToken = u ^. stripeToken
  , Domain._fuFacebookToken = u ^. facebookToken
  }

instance FromRow List where
  fromRow = List <$> field

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field

fields fs u = over each (u ^.) fs
f l = l . to toField

instance ToRow User where
  toRow = fields
    [ f username
    , f name
    , f email
    , f passwordHash
    , f cellphone
    , f avatar
    , f stripeToken
    , f facebookToken
    ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Membership where
  fromRow = Membership <$> field <*> field <*> field

instance FromRow ListTasks where
  fromRow = ListTasks <$> field <*> field
