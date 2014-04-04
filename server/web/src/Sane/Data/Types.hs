{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Sane.Data.Types where
import           Control.Lens.TH
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField hiding (name)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
-- import Facebook (AccessToken(..))
-- import qualified Facebook as F
import Prelude (Enum(..))

import Sane.Common hiding (Action)
import qualified Sane.Models.Accounts as Domain
import qualified Sane.Models.Common as Domain
import qualified Sane.Models.Lists as Domain

type Id a = Int

data User = User
  { _userUsername           :: Text
  , _userName               :: Text
  , _userEmail              :: Text
  , _userPasswordHash       :: Maybe ByteString
  , _userCellphone          :: Maybe Text
  , _userAvatar             :: Maybe Text
  , _userStripeToken        :: Maybe Text
  , _userFacebookId         :: Maybe Text
  , _userFacebookToken      :: Maybe Text
  , _userFacebookExpiration :: Maybe UTCTime
  } deriving (Eq, Show)

makeFields ''User

data List = List
  { _listTitle    :: Text
  , _listIcon     :: Maybe Text
  , _listArchived :: Bool
  , _listOwner    :: Id User
  }

makeFields ''List

data Task = Task
  { taskTitle       :: Text
  , taskDeadline    :: Maybe UTCTime
  , taskDescription :: Maybe Text
  , taskAssignedTo  :: Maybe (Id User)
  } deriving (Eq, Show)

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
  fromField fd bs = do
    n <- fromField fd bs
    case rawMembershipKinds ^. at n of
      Nothing -> returnError ConversionFailed fd "Invalid MembershipKind value"
      Just k -> return k

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Membership where
  fromRow = Membership <$> field <*> field <*> field

instance FromRow ListTasks where
  fromRow = ListTasks <$> field <*> field

instance FromRow List where
  fromRow = List <$> field <*> field <*> field <*> field

instance ToRow List where
  toRow = fields
    [ f title
    , f icon
    , f archived
    , f owner
    ]

dbList :: Getter Domain.List List
dbList = to $ List
  <$> view Domain.title
  <*> view Domain.icon
  <*> view Domain.archived
  <*> view (Domain.owner . to Domain.fromId)

decimal :: Domain.Id a -> ByteString
decimal = L.toStrict . toLazyByteString . intDec . Domain.fromId
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
  , Domain._fuEmail = u ^. email
  , Domain._fuName = u ^. name
  , Domain._fuCellphone = u ^. cellphone
  , Domain._fuAvatar = u ^. avatar
  , Domain._fuStripeToken = u ^. stripeToken
  , Domain._fuFacebookAuth = (u ^. facebookToken) -- UserAccessToken <$> (F.Id <$> u ^. facebookId) <*> (u ^. facebookToken) <*> (u ^. facebookExpiration)
  }

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field

fields :: Each s t (Getting a s1 a) a => s -> s1 -> t
fields fs u = over each (u ^.) fs

f :: (Functor f, Contravariant f, Conjoined p, ToField s) => (p s (f s) -> c) -> p Action (f Action) -> c
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
    , f facebookId
    , f facebookExpiration
    ]

