{-# LANGUAGE OverloadedStrings #-}
module Sane.Web.Accounts where
import Control.Monad.Reader
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types hiding (statusCode)
import Network.Webmachine
import Web.Cookie

import Sane.Common
import qualified Sane.Data.Accounts as A
import qualified Sane.Data.Services as A
import Sane.Models
import Sane.Web.Miscellaneous (api)

setSessionCookie :: Username -> Session -> Webmachine c s ()
setSessionCookie u s = setCookie $ def { setCookieName = "SANE", setCookieValue = encodeUtf8 u <> ":" <> s ^. token}

runAccounts :: A.AccountService a -> Webmachine AppConfig s a
runAccounts x = do
  s <- view settings
  deps <- s ^! A.accountServiceDependencies
  liftIO $ A.runAccountService deps x

createUser :: Resource AppConfig s a NewUser (Result CurrentUser)
createUser = api $ \(Just newUser) -> do
  r <- runAccounts $ A.createUser newUser
  case r of
    Left err -> case err of
      UsernameExists -> do
        statusCode .= conflict409
        return $ errorResult "username already in use" ()
      StripeError -> do
        statusCode .= internalServerError500
        return $ errorResult "error with stripe" ()
    Right (s, user) -> do
      setSessionCookie (newUser ^. username) s
      statusCode .= created201
      return $ result $ CurrentUser
        { _cuUsername = user ^. username
        , _cuEmail = user ^. email
        , _cuName = user ^. name
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
signIn = api $ \(Just credentials) -> do
  mSession <- runAccounts $ A.signIn credentials
  case mSession of
    Nothing -> do
      statusCode .= conflict409
      return Nothing
    Just (s, user) -> do
      setSessionCookie (user ^. username) s
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
