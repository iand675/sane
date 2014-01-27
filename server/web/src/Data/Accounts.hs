{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Accounts where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans
import Crypto.PasswordStore
import Data.ByteString (ByteString, pack)
import Data.Pool
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import System.Random.MWC
import Web.Stripe.Customer
import Web.Stripe.Client hiding (query)
import Common
import Database.PostgreSQL.Simple.Utility
import qualified Domain.Types as D
import Data.Types
import Prelude (Integer)

class Monad m => AccountService m where
  createUser :: D.NewUser -> m (Either D.CreateUserError (D.Session, D.FullUser))
  getUser :: D.Username -> m (Maybe D.FullUser)
  signIn :: D.Username -> D.Password -> m (Maybe (D.Session, D.FullUser))
  signOut :: D.Username -> D.Session -> m ()
  getSession :: D.Username -> D.Session -> m (Maybe D.FullUser)

createUser' :: D.NewUser -> AccountServiceM (Either D.CreateUserError (D.Session, D.FullUser))
createUser' u@(D.StandardNewUser {}) = do
  sessionToken <- makeSessionToken
  withDb $ \conn -> withTransaction conn $ do
    userExists <- query conn "select 1 from users where username = ? limit 1" $ Only $ u ^. D.username
    case firstOf traverse userExists of
      Nothing -> do
        eCust <- createStripeCustomer $ u ^. D.email
        case eCust of
          Left err -> return $ Left D.StripeError
          Right token -> do
            hashed <- makePassword (u ^. D.password . to encodeUtf8) 14
            let dbUser = User
                  { _userUsername           = u ^. D.username
                  , _userName               = u ^. D.name
                  , _userEmail              = u ^. D.email
                  , _userPasswordHash       = Just hashed
                  , _userCellphone          = Nothing
                  , _userAvatar             = Nothing
                  , _userStripeToken        = Just token
                  , _userFacebookToken      = Nothing
                  , _userFacebookId         = Nothing
                  , _userFacebookExpiration = Nothing
                  }
            execute conn "insert into users (username, name, email, password_hash, cellphone, avatar, stripe_token, facebook_token, facebook_id, facebook_expiration) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" dbUser
            return $ Right (D.session sessionToken, dbUser ^. fullUser)
      Just (Only (1 :: Int)) -> return $ Left D.UsernameExists
createUser' u@(D.FacebookNewUser {}) = do
  sessionToken <- makeSessionToken
  withDb $ \conn -> withTransaction conn $ do
    -- userExists <- query conn "select * from users where facebook_id = ? limit 1" $ Only $ u ^. D.username
    return $ Left D.UsernameExists


createStripeCustomer :: MonadIO m => Text -> m (Either StripeFailure Text)
createStripeCustomer email = runStripeT (defaultConfig $ APIKey "sk_test_zjTOEStpOjvuOV0m8sVPIfLh") $ do
  c <- createCustomer Nothing Nothing (Just $ Email email) Nothing Nothing Nothing
  return $ unCustomerId $ custId c

signIn' :: Text -> D.Password -> AccountServiceM (Maybe (D.Session, D.FullUser))
signIn' username password = do
  token <- makeSessionToken
  withDb $ \conn -> do
    liftIO $ putStrLn "Finding user"
    dbUser <- one $ query conn "select id, username, name, email, password_hash, cellphone, avatar, stripe_token from users where username = ?" $ Only username
    case dbUser of
      Nothing -> return Nothing
      Just (Only userId :. dbUser) -> case dbUser ^. passwordHash of
        Nothing -> return Nothing
        Just hash -> if verifyPassword (encodeUtf8 password) hash
          then do
            liftIO $ putStrLn "Creating session"
            now <- liftIO getCurrentTime
            execute conn "insert into sessions (user_id, session_id, expiration) values (?, ?, ?)" (userId :: Int, Binary token, addUTCTime 2592000 now)
            return $ Just (D.session token, dbUser ^. fullUser)
          else logIssue >> return Nothing

logIssue :: Monad m => m ()
logIssue = return ()

nothingIfExists :: b -> ConstraintViolation -> IO (Either b a)
nothingIfExists x (UniqueViolation constraint) = return $ Left x
nothingIfExists _ e = throw e

newtype AccountServiceM a = AccountServiceM
  { fromAccountService :: ReaderT (Pool Connection, GenIO) IO a
  } deriving (Functor, Monad, Applicative)

makeSessionToken :: AccountServiceM ByteString
makeSessionToken = AccountServiceM $ do
  gen <- view _2
  liftIO $ fmap pack $ replicateM 16 $ uniform gen

withDb :: (Connection -> IO a) -> AccountServiceM a
withDb f = AccountServiceM $ do
  pool <- view _1
  liftIO $ withResource pool f

instance AccountService AccountServiceM where
  createUser = createUser'
  signIn = signIn'

runAccountService :: Pool Connection -> GenIO -> AccountServiceM a -> IO a
runAccountService p g (AccountServiceM s) = runReaderT s (p, g)

