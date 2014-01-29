{-# LANGUAGE DeriveFunctor #-}
module Sane.Data.Services where
import Control.Category
import Control.Monad.Free
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import Prelude (($))

import Sane.Models

data AccountServiceF cont
  = CreateUser NewUser (Either CreateUserError (Session, FullUser) -> cont) -- -> m (Either D.CreateUserError (D.Session, D.FullUser))
  | GetUser Username (Maybe FullUser -> cont) -- -> m (Maybe D.FullUser)
  | SignIn SignIn (Maybe (Session, FullUser) -> cont) -- -> m (Maybe (D.Session, D.FullUser))
  | SignOut Username Session cont -- -> m ()
  | GetSession Username Session (Maybe FullUser -> cont) -- -> m (Maybe D.FullUser)
  | ResetPassword Text cont -- email
  deriving (Functor)

type AccountService = Free AccountServiceF

createUser :: NewUser -> AccountService (Either CreateUserError (Session, FullUser))
createUser n = liftF $ CreateUser n id

getUser :: Username -> AccountService (Maybe FullUser)
getUser u = liftF $ GetUser u id

signIn :: SignIn -> AccountService (Maybe (Session, FullUser))
signIn s = liftF $ SignIn s id

signOut :: Username -> Session -> AccountService ()
signOut u s = liftF $ SignOut u s ()

getSession :: Username -> Session -> AccountService (Maybe FullUser)
getSession u s = liftF $ GetSession u s id

resetPassword :: Text -> AccountService ()
resetPassword e = liftF $ ResetPassword e ()

data PaymentServiceF c
  = CreatePaymentUser c

data FacebookServiceF c
  = GetFacebookUser c
