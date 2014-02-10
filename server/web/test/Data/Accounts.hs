{-# LANGUAGE OverloadedStrings #-}
module Data.Accounts where
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString, pack)
import Data.ByteString.Base64 as B64 (encode)
import Data.Monoid ((<>))
import Data.Pool
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import System.Random.MWC
import Test.Hspec

import Sane.Data.Accounts
import Sane.Data.Services
import Sane.Models
-- import           Sane.Data.Services
-- import qualified Sane.Data.Accounts as A
{-
type AccountServiceTestHarness m = RWST (AccountServiceDependencies m) ([(Text, Text, ByteString)], [(Priority, Text)]) () m
-}

makeAccountServicesPool :: IO (Pool Connection)
makeAccountServicesPool = createPool (connect defaultConnectInfo { connectPassword = "test", connectUser = "accounts_api", connectDatabase = "accounts" }) close 8 10 4

makeSessionTokenImpl :: MonadIO m => GenIO -> m ByteString
makeSessionTokenImpl = liftIO . fmap pack . replicateM 16 . uniform

serviceDeps :: GenIO -> Pool Connection -> AccountServiceDependencies IO
serviceDeps g p = AccountServiceDependencies
  { _asLogger          = \_ _ -> return ()
  , _asPublisher       = \_ _ _ -> return ()
  , _asConnectionPool  = p
  , _asTokenGenerator  = makeSessionTokenImpl g
  , _asPaymentService  = undefined
  , _asFacebookService = undefined
  }

randomUser :: GenIO -> IO NewUser
randomUser g = do
  tok <- fmap (decodeUtf8 . B64.encode) $ makeSessionTokenImpl g
  return $ StandardNewUser tok (tok <> "@example.com") tok tok

createUserSpec :: IO Spec
createUserSpec = do
  connPool <- makeAccountServicesPool
  gen <- createSystemRandom
  let run = runAccountService $ serviceDeps gen connPool
  return $ describe "create user" $ do

    describe "as standard user with correct details" $ do
      it "returns the new user" $ do
        nu <- randomUser gen
        (Right (_, u)) <- run $ createUser nu
        u ^. username `shouldBe` nu ^. username

      it "fires a \"new user\" message" $ do
        pending

      it "returns a valid session token for the user" $ do
        nu <- randomUser gen
        (Just u) <- run $ do
          (Right (s, _)) <- createUser nu
          getSession (nu ^. username) s
        u ^. username `shouldBe` nu ^. username

    describe "when username already exists" $ do
      it "returns a \"username already exists\" error" $ do
        nu <- randomUser gen
        run $ do
          (Right (_, _)) <- createUser nu
          (Left UsernameExists) <- createUser nu
          return ()

    describe "when facebook user id already exists" $ do
      it "returns the existing user" $ do
        pending

      it "returns a valid session token for the user" $ do
        pending

    describe "when stripe is not responding" $ do
      it "returns a stripe error" $ do
        pending

      it "logs an error message" $ do
        pending

    describe "as unregistered facebook user with correct details" $ do
      it "returns the new user" pending

      it "fires a \"new user\" message" pending

      it "returns a valid session token for the user" pending

getUserSpec :: IO Spec
getUserSpec = do
  connPool <- makeAccountServicesPool
  gen <- createSystemRandom
  let run = runAccountService $ serviceDeps gen connPool
  return $ describe "get user" $ do
    it "returns existing user given the correct username" $ do
      nu <- randomUser gen
      (Right (_, u)) <- run $ createUser nu
      mu <- run $ getUser $ u ^. username
      (mu ^? _Just . username) `shouldBe` Just (nu ^. username)
    it "returns nothing given an invalid username" $ do
      mu <- run $ getUser ""
      mu `shouldBe` Nothing

-- getUserTest
signInSpec :: IO Spec
signInSpec = do
  connPool <- makeAccountServicesPool
  gen <- createSystemRandom
  let run = runAccountService $ serviceDeps gen connPool
  return $ describe "sign in" $ do
    it "returns existing user given the correct credentials" $ do
      nu <- randomUser gen
      (Right (_, u)) <- run $ createUser nu
      (Just (_, u')) <- run $ signIn $ StandardSignIn (nu ^. username) (nu ^. password)
      (u ^. username) `shouldBe` (u' ^. username)
-- signOutTest
-- getSessionTest
