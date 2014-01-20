{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Accounts where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans
import Crypto.PasswordStore
import Data.ByteString (ByteString)
import Data.Pool
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Web.Stripe.Customer
import Web.Stripe.Client
import Common
import qualified Domain.Types as D
import Data.Types

newtype Session = Session { fromSession :: ByteString }

data CreateUserError = UsernameExists | StripeError

class Monad m => AccountService m where
  createUser :: Text -> D.NewUser -> m (Either CreateUserError (Session, D.FullUser))
  getUser :: D.Username -> m (Maybe D.FullUser)
  signIn :: D.Username -> D.Password -> m (Maybe (Session, D.FullUser))
  signOut :: Session -> m ()

createUser' :: Connection -> D.Password -> D.NewUser -> IO (Either CreateUserError (Session, D.FullUser))
createUser' conn p u = handle (nothingIfExists UsernameExists) $ do
  eCust <- createStripeCustomer $ u ^. D.email
  case eCust of
    Left err -> return $ Left StripeError
    Right token -> do
      hashed <- makePassword (encodeUtf8 p) 14
      let dbUser = User
            { _userUsername      = u ^. D.username
            , _userName          = u ^. D.name
            , _userEmail         = u ^. D.email
            , _userPasswordHash  = hashed
            , _userCellphone     = Nothing
            , _userAvatar        = Nothing
            , _userStripeToken   = Just token
            , _userFacebookToken = Nothing
            }
      withTransaction conn $ do
        execute conn "insert into users (username, name, email, password_hash, cellphone, avatar, stripe_token, facebook_token) values (?, ?, ?, ?, ?, ?, ?, ?)" dbUser
        return $ Right (Session "bogus", dbUser ^. fullUser)

createStripeCustomer :: MonadIO m => Text -> m (Either StripeFailure Text)
createStripeCustomer email = runStripeT (defaultConfig $ APIKey "sk_test_zjTOEStpOjvuOV0m8sVPIfLh") $ do
  c <- createCustomer Nothing Nothing (Just $ Email email) Nothing Nothing Nothing
  return $ unCustomerId $ custId c

nothingIfExists :: b -> ConstraintViolation -> IO (Either b a)
nothingIfExists x (UniqueViolation constraint) = return $ Left x
nothingIfExists _ e = throw e

newtype AccountServiceM a = AccountServiceM
  { fromAccountService :: ReaderT (Pool Connection) IO a
  } deriving (Functor, Monad, Applicative)

instance AccountService AccountServiceM where
  createUser pass u = AccountServiceM $ do
    pool <- ask
    liftIO $ withResource pool $ \conn -> createUser' conn pass u

runAccountService :: Pool Connection -> AccountServiceM a -> IO a
runAccountService p (AccountServiceM s) = runReaderT s p
