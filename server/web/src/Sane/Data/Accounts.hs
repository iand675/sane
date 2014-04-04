{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Sane.Data.Accounts (
  runAccountService,
  runAccountServiceImpl,
  AccountServiceDependencies(..),
  accountServiceDependencies,
  AccountServiceT,
  AccountServiceM,
  makeSessionToken
) where
import Control.Lens.TH
import Control.Monad.Free
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString (ByteString, pack)
import Data.Char (toLower)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lens (text)
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple
import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)
import System.Random.MWC
import Web.Stripe.Customer
import Web.Stripe.Client hiding (query)
import Prelude (Integer, undefined, fromInteger)

import Database.PostgreSQL.Simple.Utility
import Sane.Common hiding (logger)
import qualified Sane.Common as C
import qualified Sane.Models.Common as D
import qualified Sane.Models.Accounts as D
import Sane.Data.Services
import Sane.Data.Types hiding (fields, f)
import System.Logger

data AccountServiceDependencies m = AccountServiceDependencies
  { _asLogger              :: Priority -> Text -> IO ()
  , _asPublisher           :: Text -> Text -> ByteString -> IO ()
  , _asConnectionPool      :: Pool Connection
  , _asTokenGenerator      :: m ByteString
  , _asPaymentService      :: Runner PaymentService m
  , _asFacebookCredentials :: (Text, Text, Text)
  }

makeFields ''AccountServiceDependencies

accountServiceDependencies :: MonadIO m => Action m AppConfig (AccountServiceDependencies IO)
accountServiceDependencies = act $ \conf -> liftIO $ {- withResource (conf ^. rabbitConnectionPool) $ \conn -> -} do
  return $ AccountServiceDependencies
    { _asLogger = conf ^. C.logger
    , _asPublisher = \_ _ _ -> return ()
    , _asConnectionPool = conf ^. accountConnectionPool
    , _asTokenGenerator = liftIO $ fmap pack $ replicateM 16 $ uniform $ (\(G gen) -> gen) $ conf ^. randomGenerator
    , _asPaymentService = undefined
    , _asFacebookCredentials = ("Sane", "770257949654604", "05ea6802054ad3cd845c4e729a70796e")
    }

newtype AccountServiceT m a = AccountServiceM
  { fromAccountService :: ReaderT (AccountServiceDependencies m) m a
  } deriving (Functor, Monad, Applicative, MonadIO)

type AccountServiceM = AccountServiceT IO

instance Monad m => MonadReader (AccountServiceDependencies m) (AccountServiceT m) where
  ask = AccountServiceM ask
  local f m = AccountServiceM (local f $ fromAccountService m)

instance MonadTrans AccountServiceT where
  lift f = AccountServiceM $ lift f

runAccountService :: MonadIO m => AccountServiceDependencies m -> AccountService a -> m a
runAccountService deps s = (runReaderT . fromAccountService . iterM runAccountServiceImpl) s deps

runAccountServiceImpl :: MonadIO m => AccountServiceF (AccountServiceT m a) -> AccountServiceT m a
runAccountServiceImpl p = case p of
  (CreateUser u n)    -> createUser' u    >>= n
  (GetUser u n)       -> getUser' u       >>= n
  (SignIn s n)        -> signIn' s        >>= n
  (SignOut _ _ _)     -> undefined
  (GetSession u s n)  -> getSession' u s  >>= n
  (ResetPassword _ _) -> undefined

userFields :: Query
userFields = "username, name, email, password_hash, cellphone, avatar, stripe_token, facebook_token, facebook_id, facebook_expiration"

createUser' :: MonadIO m => D.NewUser -> AccountServiceT m (Either D.CreateUserError (D.Session, D.FullUser))
createUser' u@(D.StandardNewUser {}) = do
  logM <- view logger
  sessionToken <- makeSessionToken
  transactionally $ \conn -> do
    userExists <- query conn "select 1 from users where username = ? limit 1" $ Only $ u ^. D.username
    case firstOf traverse userExists of
      Just (Only (1 :: Int)) -> return $ Left D.UsernameExists
      _ -> do
        eCust <- createStripeCustomer $ u ^. D.email
        case eCust of
          Left err -> do
            logM Error $ T.pack $ show err
            return $ Left D.StripeError
          Right token -> do
            hashed <- makePassword (u ^. D.password . to encodeUtf8) 14
            let dbUser = User
                  { _userUsername           = over text toLower $ u ^. D.username
                  , _userName               = u ^. D.name
                  , _userEmail              = over text toLower $ u ^. D.email
                  , _userPasswordHash       = Just hashed
                  , _userCellphone          = Nothing
                  , _userAvatar             = Nothing
                  , _userStripeToken        = Just token
                  , _userFacebookToken      = Nothing
                  , _userFacebookId         = Nothing
                  , _userFacebookExpiration = Nothing
                  }
            now <- liftIO getCurrentTime
            let expire = expireTime now
            (Just (Only userId)) <- one $ query conn "insert into users (username, name, email, password_hash, cellphone, avatar, stripe_token, facebook_token, facebook_id, facebook_expiration) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id" dbUser
            _ <- execute conn "insert into sessions (user_id, session_id, expiration) values (?, ?, ?)" (userId :: Int, Binary sessionToken, expire)
            return $ Right (D.session sessionToken expire, dbUser ^. fullUser)
createUser' u@(D.FacebookNewUser {}) = do
  logM <- view logger
  (fbAppName, fbAppId, fbAppSecret) <- view facebookCredentials
  let creds = FB.Credentials fbAppName fbAppId fbAppSecret
  sessionToken <- makeSessionToken
  transactionally $ \conn -> do
    fbUser <- withManager $ \m -> FB.runFacebookT creds m $ FB.getUser "me" [] $ Just $ FB.UserAccessToken (u ^. D.userId . to FB.Id) (u ^. D.accessToken) (u ^. D.expirationTime . to (posixSecondsToUTCTime . fromInteger))
    userExists <- one $ query conn "select * from users where facebook_id = ? limit 1" $ Only $ u ^. D.username
    now <- liftIO getCurrentTime
    let expire = expireTime now
    case userExists of
      Nothing -> do
        eCust <- createStripeCustomer $ u ^. D.email
        case eCust of
          Left err -> do
            logM Error $ T.pack $ show err
            return $ Left D.StripeError
          Right token -> do
            let importedUser = User
                  { _userUsername           = FB.idCode $ FB.userId fbUser
                  , _userName               = maybe "" id $ FB.userName fbUser
                  , _userEmail              = maybe "" id $ FB.userEmail fbUser
                  , _userPasswordHash       = Nothing
                  , _userCellphone          = Nothing
                  , _userAvatar             = Nothing
                  , _userStripeToken        = Just token
                  , _userFacebookToken      = Just $ u ^. D.accessToken
                  , _userFacebookId         = Just $ u ^. D.userId
                  , _userFacebookExpiration = Just $ u ^. D.expirationTime . to (posixSecondsToUTCTime . fromInteger)
                  }
            (Just (Only userId)) <- one $ query conn ("insert into users (" <> userFields <> ") values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id") importedUser
            _ <- execute conn "insert into sessions (user_id, session_id, expiration) values (?, ?, ?)" (userId :: Int, Binary sessionToken, expire)
            return $ Right (D.session sessionToken expire, importedUser ^. fullUser)
      Just foundUser -> do
        return $ Right (D.session sessionToken expire, foundUser ^. fullUser)

createStripeCustomer :: MonadIO m => Text -> m (Either StripeFailure Text)
createStripeCustomer e = runStripeT (defaultConfig $ SecretKey "sk_test_zjTOEStpOjvuOV0m8sVPIfLh") $ do
  c <- createCustomer Nothing Nothing (Just $ Email e) Nothing Nothing Nothing
  return $ unCustomerId $ custId c

signIn' :: MonadIO m => D.SignIn -> AccountServiceT m (Maybe (D.Session, D.FullUser))
signIn' (D.SignIn u p) = do
  logM <- view logger
  token <- makeSessionToken
  withDb $ \conn -> do
    logM Debug "Finding user"
    let q = "select id, " <> userFields <> " from users where username = ?"
    mdbUser <- one $ query conn q $ Only u
    case mdbUser of
      Nothing -> return Nothing
      Just (Only userId :. dbUser) -> case dbUser ^. passwordHash of
        Nothing -> return Nothing
        Just hash -> if verifyPassword (encodeUtf8 p) hash
          then do
            logM Debug "Creating session"
            now <- liftIO getCurrentTime
            _ <- execute conn "insert into sessions (user_id, session_id, expiration) values (?, ?, ?)" (userId :: Int, Binary token, expireTime now)
            return $ Just (D.session token now, dbUser ^. fullUser)
          else logM Warning "Incorrect password" >> return Nothing

getUser' :: MonadIO m => D.Username -> AccountServiceT m (Maybe D.FullUser)
getUser' u = withDb $ \conn -> do
  let q = "select " <> userFields <> "from users where username = ?"
  dbUser <- one $ query conn q $ Only u
  return $ dbUser ^? _Just . fullUser

getSession' :: MonadIO m => D.Username -> D.Session -> AccountServiceT m (Maybe (D.Persisted D.FullUser))
getSession' u s = withDb $ \conn -> do
  let q = "select id, " <> userFields <> " from users inner join sessions s on id = s.user_id where s.session_id = ?"
  dbUser <- s ^!? to (Only . Binary . D.fromSession) . act (query conn q) . folded
  case dbUser of
    Nothing -> return Nothing
    Just ((Only uid) :. u') -> if u == (u' ^. username)
      then u' ^! fullUser . to (D.Persisted (D.Id uid)) . re _Just
      else return Nothing

expireTime :: UTCTime -> UTCTime
expireTime = addUTCTime 2592000

makeSessionToken :: Monad m => AccountServiceT m ByteString
makeSessionToken = view tokenGenerator >>= lift

withDb :: MonadIO m => (Connection -> IO a) -> AccountServiceT m a
withDb f = AccountServiceM $ do
  pool <- view connectionPool
  liftIO $ withResource pool f

transactionally :: MonadIO m => (Connection -> IO a) -> AccountServiceT m a
transactionally f = withDb $ \conn -> withTransaction conn (f conn)
