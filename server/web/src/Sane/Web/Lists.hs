{-# LANGUAGE OverloadedStrings #-}
module Sane.Web.Lists where
import           Control.Monad.Reader
import           Network.HTTP.Types hiding (statusCode)
import           Network.Webmachine
import           Prelude (uncurry)

import           Sane.Common
import qualified Sane.Data.Accounts as A
import qualified Sane.Data.Tasks as T
import qualified Sane.Data.Services as S
import           Sane.Models
import           Sane.Web.Miscellaneous (api, getSaneCookie)

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

createList :: Resource AppConfig s a List (Result (Persisted List))
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
          r <- runTasks $ S.createList uid list
          return $ result r

{-getList :: Id List -> ListAction List-}
{-getList = undefined-}

listLists :: Resource AppConfig s a List (Result [Persisted List])
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
          r <- runTasks $ S.getLists uid
          return $ result r

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
