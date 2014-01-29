{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
module Sane.Routes where
import Control.Category ((.))
import Control.Monad.Writer
import Data.Text (Text)
import Network.HTTP.Types.Method
import Network.Webmachine.Routing
import Text.Boomerang
import Text.Boomerang.TH
import Prelude (($), snd, Eq, Show)

newtype Id a = Id { fromId :: Text }
  deriving (Show, Eq)

type Username = Text
type ListId = Id ()
type TaskId = Id ()

data SaneAction
  = SignIn
  | SignOut
  | CreateUser
  | ListUsers
  | GetUser { routeUsername :: Username }
  -- list actions
  | CreateList
  | ListLists
  | UpdateList { listId :: ListId }
  | DeleteList { listId :: ListId }
  -- membership
  | CreateMembership { listId :: ListId, memberUsername :: Username }
  | UpdateMembership { listId :: ListId, memberUsername :: Username }
  | DeleteMembership { listId :: ListId, memberUsername :: Username }
  -- tasks
  | ListTasks  { listId :: ListId }
  | CreateTask { listId :: ListId }
  | UpdateTask { listId :: ListId, taskId :: TaskId }
  | DeleteTask { listId :: ListId, taskId :: TaskId }
  deriving (Eq, Show)

makeBoomerangs ''SaneAction

register :: Writer (Boomerang RouteError (Method, [Text]) a b) a -> Boomerang RouteError (Method, [Text]) a b
register = snd . runWriter

saneRoutes :: Router SaneAction
saneRoutes = register $ do
  route POST   $ rSignIn . "signin"
  route POST   $ rSignOut . "signout"
  route POST   $ rCreateUser . "users"
  route GET    $ rListUsers . "users"
  -- route GET    $ rGetUser . "users" </> anyText
  route POST   $ rCreateList . "lists"
  route GET    $ rListLists . "lists"
  -- route PATCH  $ rUpdateList . "lists" </> anyText
  -- route DELETE $ rDeleteList . "lists" </> anyText
