{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Router where
import           Data.Text (Text)
import           Prelude hiding ((.), id)
import           Control.Category ((.), id)
import           Control.Monad.Writer
import           Network.HTTP.Types
import           Text.Boomerang.Error       (ParserError(..),ErrorMsg(..), (<?>), condenseErrors, mkParserError)
import           Text.Boomerang.HStack      ((:-)(..), arg)
import           Text.Boomerang.Prim        (Parser(..), Boomerang(..), parse1, xmaph, xpure, unparse1, val)
import           Text.Boomerang.Pos
import           Text.Boomerang.Texts
import           Text.Boomerang.TH


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
  | GetUser Username
  -- list actions
  | CreateList
  | ListLists
  | UpdateList ListId
  | DeleteList ListId
  -- membership
  | CreateMembership ListId Username
  | UpdateMembership ListId Username
  | DeleteMembership ListId Username
  -- tasks
  | ListTasks  ListId
  | CreateTask ListId
  | UpdateTask ListId TaskId
  | DeleteTask ListId TaskId
  deriving (Eq, Show)

makeBoomerangs ''SaneAction

class ToMethod m where
  toMethod :: m -> Method

instance ToMethod StdMethod where
  toMethod = renderStdMethod

type RouteError = ParserError (Method, MajorMinorPos) -- ParserError (Method, MajorMinorPos)

instance Functor ParserError where
  fmap f (ParserError mp msg) = ParserError (fmap f mp) msg

instance InitialPosition (ParserError (Method, MajorMinorPos)) where
  initialPos _ = (methodGet, MajorMinorPos 0 0)

type Router a = Boomerang RouteError (Method, [Text]) () (a :- ())

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

route :: ToMethod m => m -> Boomerang TextsError [Text] a b -> Writer (Boomerang RouteError (Method, [Text]) a b) ()
route method (Boomerang (Parser p) sr) = tell $ Boomerang (Parser $ wrapParser p) (wrapUnparser sr)
  where
    m = toMethod method
    -- f :: [Text] -> Pos TextsError -> [Either TextsError ((a, [Text]), Pos TextsError)]
    -- f' :: (Method, [Text]) -> Pos RouteError -> [Either RouteError (a, (Method, [Text]), Pos RouteError)]
    wrapParser f = f'
      where
        f' (m', t) p = map (convertResults m') $ f t (snd p)
        convertResults m' r = case r of
          Left p -> Left $ fmap (\e -> (m', e)) p
          Right ((a, ts), p) -> if m /= m'
            then Left $ ParserError (Just (m', p)) []
            else Right ((a, (m', ts)), (m', p))
    wrapUnparser f = map (\(tf, a) -> ((\t -> (m, t)) . tf . snd, a)) . f

parseRoute :: Boomerang RouteError (Method, [Text]) () (r :- ()) -> Method -> [Text] -> Either RouteError r
parseRoute pp m strs = either (Left . condenseErrors) Right $ parse1 (\(meth, r) -> meth == m && isComplete r) pp (m, strs)

unparseRoute :: Boomerang RouteError (Method, [Text]) () (r :- ()) -> r -> Maybe (Method, [Text])
unparseRoute = unparse1 (methodGet, [])

