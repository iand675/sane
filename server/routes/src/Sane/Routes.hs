{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Sane.Routes where
import Control.Category ((.))
import Control.Monad.Writer
import Data.Functor
import Data.Maybe
import Data.Text (Text, intercalate)
import Network.HTTP.Types.Method
import Network.Webmachine.Routing
import Text.Boomerang
import Text.Boomerang.TH
import Text.Boomerang.Texts
import Prelude (($), snd, Eq, Show, Int)

newtype Id a = Id { fromId :: Text }
  deriving (Show, Eq)

type Username = Text
type ListId = Int
type TaskId = Id ()

data SaneAction
  = SignIn
  | SignOut
  | CreateUser
  | ListUsers
  | GetCurrentUser
  | GetUser { routeUsername :: Username }
  -- list actions
  | CreateList
  | ListLists
  | GetList    { listId :: ListId }
  | UpdateList { listId :: ListId }
  | DeleteList { listId :: ListId }
  -- tasks
  | ListTasks
  | CreateTask
  | GetTask    { taskId :: TaskId }
  | UpdateTask { taskId :: TaskId }
  | DeleteTask { taskId :: TaskId }
  -- miscellaneous
  | Ping
  | TrackEvent
  | TrackError
  deriving (Eq, Show)

makeBoomerangs ''SaneAction

register :: Writer (Boomerang RouteError (Method, [Text]) a b) a -> Boomerang RouteError (Method, [Text]) a b
register = snd . runWriter

saneRoutes :: Router SaneAction
saneRoutes = register $ do
  route GET    $ rPing . "ping"
  route POST   $ rSignIn . "signin"
  route POST   $ rSignOut . "signout"
  route POST   $ rCreateUser . "users"
  route GET    $ rListUsers . "users"
  route GET    $ rGetCurrentUser . "account"
  -- route GET    $ rGetUser . "users" </> anyText
  route POST   $ rCreateList . "lists"
  route GET    $ rListLists . "lists"
  route GET    $ rGetList . "lists" </> int
  -- route PATCH  $ rUpdateList . "lists" </> anyText
  -- route DELETE $ rDeleteList . "lists" </> anyText
  route GET    $ rListTasks . "tasks"
  route POST   $ rCreateTask . "tasks"
  route POST   $ rTrackEvent . "metrics" </> "events"
  route POST   $ rTrackEvent . "metrics" </> "errors"

saneRoute :: SaneAction -> Maybe (Method, Text)
saneRoute = fmap concatRoute . unparseRoute saneRoutes
  where concatRoute (m, r) = (m, intercalate "/" r)
