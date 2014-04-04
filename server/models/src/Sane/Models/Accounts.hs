{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sane.Models.Accounts where
import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64 (encode, decodeLenient)
import           Data.Time (UTCTime)
import           Data.Text (Text)
import           Data.ProtocolBuffers.Lens
import           Network.HTTP.JSON.API
import           Sane.Models.Common
import           Sane.Models.JSON
import           Sane.Routes hiding (Username)
import qualified Sane.Wire.Accounts as W

type Username = Text
type Password = Text

-- ************************************************************************************

data FullUser = FullUser
  { _fuUsername      :: Username
  , _fuName          :: Text
  , _fuEmail         :: Text
  , _fuCellphone     :: Maybe Text
  , _fuAvatar        :: Maybe Text
  , _fuStripeToken   :: Maybe Text
  , _fuFacebookAuth  :: Maybe Text
  } deriving (Eq, Show)

makeFields ''FullUser
-- ************************************************************************************

data User = User
  { _uUsername :: Username
  , _uName     :: Text
  , _uAvatar   :: Maybe Text
  } deriving (Eq, Show)

makeFields ''User
jsonize ''User

instance ToModel FullUser User where
  toModel = to getUser
    where
      getUser = User
        <$> view username
        <*> view name
        <*> view avatar

{-
instance JsonAPIDocument User where
  documentType = to $ const "user"
  toDocument = to $ \user ->
    let n = user ^. username in
    doc user
      & textId ?~ n
      & href .~ saneRoute (n ^. re _GetUser)
-}
-- ************************************************************************************

data CurrentUser = CurrentUser
  { _cuUsername  :: Text
  , _cuEmail     :: Text
  , _cuName      :: Text
  , _cuCellphone :: Maybe Text
  , _cuAvatar    :: Maybe Text
  }

makeFields ''CurrentUser
jsonize ''CurrentUser

instance ToModel FullUser CurrentUser where
  toModel = to getUser
    where
      getUser = CurrentUser
        <$> view username
        <*> view email
        <*> view name
        <*> view cellphone
        <*> view avatar

instance JsonAPIDocument CurrentUser where
  documentType = to $ const "user"

currentUser :: Getter CurrentUser (Document CurrentUser)
currentUser = to $ \u -> doc u
  & documentId ?~ u ^. username . re textId
  & documentHref .~ saneRoute GetCurrentUser ^? _Just . _2

-- ************************************************************************************

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
jsonize ''NewUser

-- ************************************************************************************

data SignIn
  = SignIn
    { _siUsername :: Username
    , _siPassword :: Password
    }

makeFields ''SignIn
jsonize ''SignIn

-- ************************************************************************************

data Session = Session
  { _sessionToken :: ByteString
  , _sessionExpiration :: UTCTime
  }

makeFields ''Session

session tok time = Session (B64.encode tok) time
fromSession s = s ^. token . to B64.decodeLenient

-- ************************************************************************************

data CreateUserError = UsernameExists | StripeError

