{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sane.Messaging.Queue where
import Control.Lens
import Control.Monad
import Data.Default
import Data.Map
import Data.Monoid
import Data.Text (Text)
import qualified Network.AMQP as A
import qualified Network.AMQP.Types as A
import Pipes
import qualified Pipes.Concurrent as C
import qualified Pipes.Prelude as P
import Pipes.AMQP
import Prelude (Show, IO, Int, Bool(..), Either(..), Maybe(..), putStrLn, ($), show, ($!), (.))
import Sane.Messaging.Routes

newtype Queue a = Queue { _qName :: Text }
  deriving (Show)

namedQueue :: Text -> Queue a
namedQueue = Queue

unnamedQueue :: Queue a
unnamedQueue = Queue ""

data QueueOptions = QueueOptions
  { _qoPassive    :: Bool
  , _qoDurable    :: Bool
  , _qoExclusive  :: Bool
  , _qoAutoDelete :: Bool
  , _qoHeaders    :: Map Text A.FieldValue
  }

makeFields ''QueueOptions
makeFields ''Queue

instance Default QueueOptions where
  def = QueueOptions False True False False mempty

declareQueue :: A.Channel -> Queue a -> QueueOptions -> IO (Queue a, Int, Int)
declareQueue chan q opts = do
  (qn, mc, cc) <- A.declareQueue chan $ A.QueueOpts
    { A.queueName = q ^. name
    , A.queuePassive    = opts ^. passive
    , A.queueDurable    = opts ^. durable
    , A.queueExclusive  = opts ^. exclusive
    , A.queueAutoDelete = opts ^. autoDelete
    , A.queueHeaders    = opts ^. headers . to A.FieldTable
    }
  return $! (Queue qn, mc, cc)

bindQueue :: (Show a, Exchange e a) => A.Channel -> Queue a -> e -> [a] -> IO ()
bindQueue c q e ts = forM_ ts $ \t -> case topic e t of
  Nothing -> putStrLn (show t)
  Just tn -> A.bindQueue c qn en tn
  where
    qn = q ^. name
    en = exchange e

class Message a where
  toMessage :: a -> A.Message
  fromMessage :: (A.Message, A.Envelope) -> Maybe a

consumeMessages :: Message a => A.Channel -> A.Ack -> C.Buffer (A.Message, A.Envelope) -> Queue a -> IO (MessageReceiver, Producer (Either (A.Message, A.Envelope) a) IO ())
consumeMessages c a b q = do
  (r, p) <- receiveMessages c a b $ _qName q
  return (r, p >-> P.map convertMessage)
  where
    convertMessage x = case fromMessage x of
                         Nothing -> Left x
                         Just m' -> Right m'

