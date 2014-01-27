module Web.Accounts where
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Accounts as A
import Data.Text.Encoding (encodeUtf8)
import Common
import Domain.Types
import Router (SaneAction(..))
import Web.Context
import Webmachine
import Web.Cookie
import Web.Types

setSessionCookie :: Username -> Session -> Webmachine c s ()
setSessionCookie username session = setCookie $ def { setCookieName = "SANE", setCookieValue = encodeUtf8 username <> ":" <> session }

api :: (ToJSON responseBody, FromJSON body) => (body -> Webmachine c s responseBody) -> Resource c s a body responseBody
api = supportJSON . basic

runAccounts :: A.AccountServiceM a -> Webmachine AppConfig s a
runAccounts x = do
  pool <- view (settings . accountConnectionPool)
  (G gen) <- view (settings . randomGenerator)
  liftIO $ A.runAccountService pool gen x

createUser :: Resource AppConfig s a NewUser CurrentUser
createUser = api $ \newUser -> do
  result <- runAccounts $ A.createUser newUser
  case result of
    -- Left _ -> freak out
    Right (session, user) -> do
      setSessionCookie (newUser ^. username) session
      return $ CurrentUser
        { _cuUsername = newUser ^. username
        , _cuEmail = newUser ^. email
        , _cuName = newUser ^. name
        , _cuAvatar = Nothing
        , _cuCellphone = Nothing
        }

{-
-- getCurrentUser :: AccountResource (Maybe FullUser) Void 
getCurrentUser = basic $ \_ -> do
  session <- Account.getSession =<< getCookie
  return $ session ^. user
-}

signIn :: Resource AppConfig s a SignIn (Maybe CurrentUser)
signIn = api $ \credentials -> do
  mSession <- runAccounts $ A.signIn (credentials ^. username) (credentials ^. password)
  case mSession of
    -- Nothing -> return badRequest
    Just (session, user) -> do
      setSessionCookie (user ^. username) session
      return $ Just $ CurrentUser
        { _cuUsername = user ^. username
        , _cuEmail = user ^. email
        , _cuName = user ^. name
        , _cuAvatar = Nothing
        , _cuCellphone = Nothing
        }

{-
-- signOut :: AccountAction ()
signOut = basic $ \_ -> do
  c <- getCookie c
  getCurrentUser
  getSession c
  if _id currentUser == _id session
    then Account.deleteSession c
    else return notAuthorized
-}
