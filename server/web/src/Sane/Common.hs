{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Sane.Common (
  module Control.Applicative,
  module Control.Category,
  module Control.Lens,
  module Control.Monad,
  module Data.Bool,
  module Data.Either,
  module Data.Eq,
  module Data.Int,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Void,
  module Text.Show,
  IO.IO,
  (P.$),
  AppConfig(..),
  accountConnectionPool,
  stripeAPIKey,
  randomGenerator,
  rabbitConnectionPool,
  minLogLevel,
  metricRegistry,
  persistedDoc,
  taskConnectionPool,
  logger,
  RandomGen(..),
  Runner(..)
) where
import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Data.Aeson (ToJSON)
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Metrics.Registry
import           Data.Pool (Pool)
import           Data.Text (Text)
import           Data.Void
import           Database.PostgreSQL.Simple (Connection)
-- import qualified Database.Redis as R
import qualified Network.AMQP as A
import qualified Network.HTTP.JSON.API as API
import qualified System.IO as IO
import           System.Random.MWC (GenIO)
import           Text.Show
import qualified Prelude as P
import           Prelude (($))
import           Web.Stripe.Client (SecretKey)

import           System.Logger
import qualified Sane.Models.Common as M

newtype RandomGen = G GenIO

data AppConfig = AppConfig
  { _acAccountConnectionPool :: Pool Connection
  , _acTaskConnectionPool    :: Pool Connection
  , _acStripeAPIKey          :: SecretKey
  , _acRandomGenerator       :: RandomGen
  , _acRabbitConnectionPool  :: Pool A.Connection
  , _acMinLogLevel           :: Priority
  , _acMetricRegistry        :: MetricRegistry P.IO
  , _acLogger                :: Priority -> Text -> P.IO ()
  -- , _acRedisConnection       :: R.Connection
  }

newtype Runner m m' = Runner { run :: forall a. m a -> m' a }

makeFields ''AppConfig

persistedDoc :: ToJSON a => Getter (M.Persisted a) (API.Document a)
persistedDoc = to $ \pa -> API.doc (pa ^. M.value) & API.documentId . _Just . API.intId .~ pa ^. M.id . to M.fromId
