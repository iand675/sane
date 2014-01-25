{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Webmachine where
import           Blaze.ByteString.Builder (toByteString)
import           Control.Lens.TH
import           Control.Monad.RWS
import           Data.Aeson (ToJSON, FromJSON, encode, decode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (Source)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import           Data.Text (Text)
import           Network.HTTP.Types
import           Network.Wai.Lens
import           Web.Cookie
import           Common

data Body = LazyByteString L.ByteString
type AuthHeader = ()

data WebmachineContext a = WebmachineContext
  { _contextSettings :: a
  }

data WebmachineState s = WebmachineState
  { _stateRequest :: Request
  , _stateStatusCode :: Status
  , _stateResponseHeaders :: ResponseHeaders
  , _stateUserState :: s
  }

type Webmachine c s = RWST (WebmachineContext c) () (WebmachineState s) IO

data Haltable a = ErrorResponse
                | StatusResponse
                | Continue a

data Resource c s auth body responseBody = Resource
  { _rServiceAvailable :: Webmachine c s (Haltable Bool)
  , _rUriTooLong :: Webmachine c s (Haltable Bool)
  , _rSupportedInputContentTypes :: H.HashMap ByteString (Request -> Webmachine c s (Maybe body))
  , _rSupportedOutputContentTypes :: H.HashMap ByteString (responseBody -> Body)
  , _rAuthorization :: Webmachine c s (Haltable (Either AuthHeader Bool))
  , _rHandler :: body -> Webmachine c s responseBody
  }

makeFields ''WebmachineContext
makeFields ''WebmachineState
makeFields ''Resource

decodeInput :: Resource c s a body r -> Webmachine c s (Maybe body)
decodeInput res = do
  req <- use request
  let mDecoder = contentTypeHeader >>= supportedTypes
      supportedTypes x = H.lookup x $ res ^. supportedInputContentTypes
      contentTypeHeader = L.lookup hContentType $ req ^. requestHeaders
  case mDecoder of
    Nothing -> return Nothing
    Just h -> h req

encodeOutput :: Resource c s a body r -> r -> Webmachine c s Body
encodeOutput res resp = do
  req <- use request
  let mEncoder = acceptHeader >>= supportedTypes
      supportedTypes x = H.lookup x $ res ^. supportedOutputContentTypes
      acceptHeader = L.lookup hAccept $ req ^. requestHeaders
  case mEncoder of
    Nothing -> return $ maybe (LazyByteString "no supported accept type") ($ resp) $ res ^? supportedOutputContentTypes . folded
    Just f -> return $ f resp

basic :: (body -> Webmachine c s responseBody) -> Resource c s auth body responseBody
basic handler = Resource
  { _rServiceAvailable = return $ Continue True
  , _rUriTooLong = return $ Continue False
  , _rSupportedInputContentTypes = mempty
  , _rSupportedOutputContentTypes = mempty
  , _rAuthorization = return $ Continue $ Right True
  , _rHandler = handler
  }

supportJSON :: (FromJSON body, ToJSON responseBody) => Resource c s auth body responseBody -> Resource c s auth body responseBody
supportJSON r = r
  & supportedInputContentTypes . at "application/json" ?~ (perform $ lazyRequestBody . to decode)
  & supportedOutputContentTypes . at "application/json" ?~ (LazyByteString . encode)

runResource :: Resource c s auth body responseBody -> Webmachine c s Body
runResource r = do
  mIn <- decodeInput r
  case mIn of
    Nothing -> return $ LazyByteString "bad request"
    Just _in -> do
      result <- r ^. handler $ _in
      _out <- encodeOutput r result
      return _out

runWebmachine :: c -> s -> Request -> Webmachine c s a -> IO a
runWebmachine c s r m = fmap (^. _1) $ evalRWST m (WebmachineContext c) (WebmachineState r ok200 [] s)

makeResponse :: Body -> Webmachine c s Response
makeResponse (LazyByteString bs) = do
  code <- use Webmachine.statusCode
  headers <- use Webmachine.responseHeaders
  return $ responseLBS code headers bs

setCookie :: SetCookie -> Webmachine c s ()
setCookie s = Webmachine.responseHeaders %= (("Set-Cookie", toByteString $ renderSetCookie s) :)
