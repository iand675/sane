{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Sane.Wire.Accounts where
import           Data.ByteString (ByteString)
import           Data.ProtocolBuffers
import           Data.Text
import           Data.TypeLevel
import           GHC.Generics (Generic)
import           Sane.Wire.Common

-- ************************************************************************************

data FullUser = FullUser
  { userUsername      :: Required D1 (Value Text)
  , userName          :: Required D2 (Value Text)
  , userEmail         :: Required D3 (Value Text)
  , userCellphone     :: Optional D4 (Value Text)
  , userAvatar        :: Optional D5 (Value Text)
  , userStripeToken   :: Optional D6 (Value Text)
  -- , _userFacebookAuth  :: Optional D7 (Message FacebookAuth)
  } deriving (Generic, Show)

instance Encode FullUser
instance Decode FullUser

-- ************************************************************************************

type PersistedFullUser = Persisted FullUser

data Session = Session
  { sessionToken      :: Required D1 (Value ByteString)
  , sessionUserId     :: Required D2 (Value Id)
  -- , _sessionExpiration :: Optional D3 (Value POSIXTime)
  } deriving (Generic, Show)

instance Encode Session
instance Decode Session

-- ************************************************************************************
