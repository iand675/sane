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
  | GetSession Username Session (Maybe (Persisted FullUser) -> cont) -- -> m (Maybe D.FullUser)
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

getSession :: Username -> Session -> AccountService (Maybe (Persisted FullUser))
getSession u s = liftF $ GetSession u s id

resetPassword :: Text -> AccountService ()
resetPassword e = liftF $ ResetPassword e ()

data PaymentServiceF c
  = CreatePaymentUser c

type PaymentService = Free PaymentServiceF

data TaskServiceF cont
  = CreateList (Id User) List (Persisted List -> cont)
  | GetLists (Id User) ([Persisted List] -> cont)
  | CreateTask cont
  | GetTasks cont
  | UpdateTask cont
  deriving (Functor)

type TaskService = Free TaskServiceF

createList :: Id User -> List -> TaskService (Persisted List)
createList uid l = liftF $ CreateList uid l id

getLists :: Id User -> TaskService [Persisted List]
getLists u = liftF $ GetLists u id

