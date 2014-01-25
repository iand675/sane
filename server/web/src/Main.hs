{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Pool
import           Database.PostgreSQL.Simple (connect, close, defaultConnectInfo, connectPassword, connectUser, connectDatabase)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Prelude (undefined)
import           System.Random.MWC
import           Web.Stripe.Client (APIKey(..))

import Common
import Router
import qualified Web.Accounts as A
import Webmachine

makeAccountServicesPool = createPool (connect defaultConnectInfo { connectPassword = "test", connectUser = "accounts_api", connectDatabase = "accounts" }) close 8 10 4

main :: IO ()
main = do
  asPool <- makeAccountServicesPool
  gen <- createSystemRandom
  let conf = AppConfig asPool (APIKey "sk_test_zjTOEStpOjvuOV0m8sVPIfLh") (G gen)
  Warp.run 3000 $ \request -> do
    case parseRoute saneRoutes (requestMethod request) $ pathInfo request of
      Left err -> return $ responseLBS notFound404 [] "unrecognized resource"
      Right action -> handleAction conf () request action

handleAction c s r action = runWebmachine c s r $ makeResponse =<< case action of
  SignIn -> runResource $ A.signIn
  SignOut -> undefined
  CreateUser -> runResource $ A.createUser
  {-
  ListUsers
  GetUser
  CreateList
  ListLists
  UpdateList
  DeleteList
  CreateMembership
  ListTasks
  CreateTask
  UpdateTask
  DeleteTask
  -}
