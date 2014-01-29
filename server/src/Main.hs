{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Pool
import           Database.PostgreSQL.Simple (connect, close, defaultConnectInfo, connectPassword, connectUser, connectDatabase)
import           Network.HTTP.Types
import           Network.Webmachine
import           Network.Webmachine.Routing
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Lens
import           Prelude (undefined)
import           System.Random.MWC
import           Web.Stripe.Client (SecretKey(..))

import Sane.Common
import Sane.Routes
import qualified Sane.Web.Accounts as A

makeAccountServicesPool = createPool (connect defaultConnectInfo { connectPassword = "test", connectUser = "accounts_api", connectDatabase = "accounts" }) close 8 10 4

main :: IO ()
main = do
  asPool <- makeAccountServicesPool
  gen <- createSystemRandom
  let conf = AppConfig asPool (SecretKey "sk_test_zjTOEStpOjvuOV0m8sVPIfLh") (G gen)
  saneHeader
  Warp.run 3000 $ \request -> do
    case parseRoute saneRoutes (request ^. method) (request ^. pathInfo) of
      Left err -> return $ responseLBS notFound404 [] "unrecognized resource"
      Right action -> handleAction conf () request action

handleAction c s r action = runWebmachine c s r $ makeResponse =<< case action of
  SignIn -> runResource $ A.signIn
  SignOut -> undefined
  CreateUser -> runResource $ A.createUser

saneHeader :: IO ()
saneHeader = C.putStrLn "_____________ ____________ \n__  ___/  __ `/_  __ \\  _ \\\n_(__  )/ /_/ /_  / / /  __/\n/____/ \\__,_/ /_/ /_/\\___/"
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
