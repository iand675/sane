module Web.Accounts where
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Accounts as A
import Common
import Domain.Types
import Router (SaneAction(..))
import Web.Context
import Webmachine
import Web.Types

api :: (ToJSON responseBody, FromJSON body) => (body -> Webmachine c s responseBody) -> Resource c s a body responseBody
api = supportJSON . basic

runAccounts :: A.AccountServiceM a -> Webmachine AppConfig s a
runAccounts x = do
  pool <- view (settings . accountConnectionPool)
  liftIO $ A.runAccountService pool x

createUser :: Resource AppConfig s a NewUser CurrentUser
createUser = api $ \newUser -> do
  (Right (session, user)) <- runAccounts $ do
    A.createUser (newUser ^. password) $ newUser
  return $ CurrentUser
    { _cuUsername = newUser ^. username
    , _cuEmail = newUser ^. email
    , _cuName = newUser ^. name
    , _cuAvatar = Nothing
    , _cuCellphone = Nothing
    }

{-
-- createUser :: AccountResource Void NewUser FullUser
createUser = basic $ \_ -> do
  newAccount <- use (request . body)
  newUser <- Account.createUser newAccount
  session <- Account.createSession (body ^. username) (body ^. password)
  setCookie c session
  return newUser

-- getCurrentUser :: AccountResource (Maybe FullUser) Void 
getCurrentUser = basic $ \_ -> do
  session <- Account.getSession =<< getCookie
  return $ session ^. user

-- signIn :: Username -> Password -> AccountAction (Maybe User)
signIn username password = basic $ \_ -> do
  session <- Account.createSession username password
  if session == Nothing
    then return badRequest
    else do
      setCookie c session
      getCurrentUser

-- signOut :: AccountAction ()
signOut = basic $ \_ -> do
  c <- getCookie c
  getCurrentUser
  getSession c
  if _id currentUser == _id session
    then Account.deleteSession c
    else return notAuthorized
-}
