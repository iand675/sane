module Data.Accounts where
import Test.Hspec

-- import           Sane.Data.Services
-- import qualified Sane.Data.Accounts as A
{-
type AccountServiceTestHarness m = RWST (AccountServiceDependencies m) ([(Text, Text, ByteString)], [(Priority, Text)]) () m

makeAccountServicesPool = createPool (connect defaultConnectInfo { connectPassword = "test", connectUser = "accounts_api", connectDatabase = "accounts" }) close 8 10 4

makeSessionToken :: MonadIO m => GenIO -> m ByteString
makeSessionToken = liftIO . fmap pack . replicateM 16 . uniform

serviceDeps :: GenIO -> Pool Connection -> AccountServiceDependencies IO
serviceDeps g p = AccountServiceDependencies
  { _asLogger          = 
  , _asPublisher       = 
  , _asConnectionPool  = p
  , _asTokenGenerator  = makeSessionToken g
  , _asPaymentService  = 
  , _asFacebookService = 
  }
-}

createUserSpec :: Spec
createUserSpec = do
  -- connPool <- makeAccountServicesPool
  -- gen <- createSystemRandom
  describe "create user" $ do
    describe "when username already exists" $ do
      it "returns a \"username already exists\" error" $ --run $ do
        {-
        let nu = NewUser "test" "test@example.com" "test" "test"
        (Right (_, u)) <- createUser nu
        (Left UsernameExists) <- createUser nu
        -}
        pending
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
    describe "as standard user with correct details" $ do
      it "returns the new user" $ do
        pending
      it "fires a \"new user\" message" $ do
        pending
      it "returns a valid session token for the user" $ do
        pending

    describe "as unregistered facebook user with correct details" $ do
      it "returns the new user" pending
      it "fires a \"new user\" message" pending
      it "returns a valid session token for the user" pending

-- getUserTest
-- signInTest
-- signOutTest
-- getSessionTest
