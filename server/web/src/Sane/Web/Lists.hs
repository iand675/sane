{-# LANGUAGE OverloadedStrings #-}
module Sane.Web.Lists where
import           Control.Monad.Reader
import           Data.Monoid
import           Network.HTTP.JSON.API
import           Network.HTTP.Types hiding (statusCode)
import           Network.Webmachine
import           Prelude (uncurry)

import           Sane.Common hiding (id)
import qualified Sane.Data.Accounts as A
import qualified Sane.Data.Tasks as T
import qualified Sane.Data.Services as S
import           Sane.Models.Common
import           Sane.Models.Lists
import           Sane.Web.Miscellaneous (api, getSaneCookie)

listDoc :: Getter (Persisted List) (Document List)
listDoc = to $ \l -> doc (l ^. value)
  & documentId ?~ (l ^. id . to fromId . re intId)
  & documentLinks ?~ (mempty & at "owner" ?~ (l ^. value . owner . to (One . LinkId . LinkIntId . fromId)))

listsRoot :: Getter [Persisted List] (Root List)
listsRoot = to $ \rs -> 
  roots' "lists" (over each (^. listDoc) rs)
    & rootLinks ?~ (mempty & at "lists.owner" ?~ UrlTemplateObject "/users/{lists.owner}" (Just "users"))

runTasks :: S.TaskService a -> Webmachine AppConfig s a
runTasks x = do
  s <- view settings
  deps <- s ^! T.taskServiceDependencies
  liftIO $ T.runTaskService deps x

runAccounts :: S.AccountService a -> Webmachine AppConfig s a
runAccounts x = do
  s <- view settings
  deps <- s ^! A.accountServiceDependencies
  liftIO $ A.runAccountService deps x

createList :: Resource AppConfig s a List (Result List)
createList = api $ \(Just list) -> do
  mSession <- getSaneCookie
  case mSession of
    Nothing -> do
      statusCode .= unauthorized401
      return $ errorResult "not authorized" ()
    Just s' -> do
      s <- runAccounts $ uncurry S.getSession $ s'
      case s of
        Nothing -> do
          statusCode .= unauthorized401
          return $ errorResult "not authorized" ()
        Just (Persisted uid _) -> do
          statusCode .= created201
          r <- runTasks $ S.createList (uid ^. to (Id . fromId)) list
          [r] ^! listsRoot . to result

{-getList :: Id List -> ListAction List-}
{-getList = undefined-}

listLists :: Resource AppConfig s a List (Result List)
listLists = api $ \Nothing -> do
  mSession <- getSaneCookie
  case mSession of
    Nothing -> do
      statusCode .= unauthorized401
      return $ errorResult "not authorized" ()
    Just s' -> do
      s <- runAccounts $ uncurry S.getSession $ s'
      case s of
        Nothing -> do
          statusCode .= unauthorized401
          return $ errorResult "not authorized" ()
        Just (Persisted uid _) -> do
          uid ^! to (Id . fromId) . act (runTasks . S.getLists) . listsRoot . to result

{-
updateList :: Id List -> ListPatch -> ListAction List
updateList = undefined

deleteList :: Id List -> ListAction Void
deleteList = undefined

getMembership :: Id List -> Username -> ListAction Membership
getMembership = undefined

setMembership :: Id List -> Username -> MembershipKind -> ListAction Membership
setMembership = undefined

deleteMembership :: Id List -> Username -> ListAction Membership
deleteMembership = undefined
-}
