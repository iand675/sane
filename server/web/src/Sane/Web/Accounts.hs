{-# LANGUAGE OverloadedStrings #-}
module Sane.Web.Accounts where
import Control.Monad.Reader
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)
import Network.Webmachine
import Web.Cookie

import Sane.Common
import qualified Sane.Data.Accounts as A
import qualified Sane.Data.Services as A
import Sane.Models
import Sane.Routes (SaneAction(..))
import Sane.Web.Types

setSessionCookie :: Username -> Session -> Webmachine c s ()
setSessionCookie username session = setCookie $ def { setCookieName = "SANE", setCookieValue = encodeUtf8 username <> ":" <> session ^. token}

api :: (ToJSON responseBody, FromJSON body) => (body -> Webmachine c s responseBody) -> Resource c s a body responseBody
api = supportJSON . basic

runAccounts :: A.AccountService a -> Webmachine AppConfig s a
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
  mSession <- runAccounts $ A.signIn credentials
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
