{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
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
  RandomGen(..),
  Runner,
  Runners(..),
) where
import Control.Applicative
import Control.Category
import Control.Lens
import Control.Lens.TH (makeFields)
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Bool
import Data.Either
import Data.Eq
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Data.Text (Text)
import Data.Void
import qualified System.IO as IO
import System.Random.MWC (GenIO)
import Text.Show
import qualified Prelude as P
import Web.Stripe.Client (SecretKey)

import Sane.Data.Services
import System.Logger

newtype RandomGen = G GenIO

data AppConfig = AppConfig
  { _acAccountConnectionPool :: Pool Connection
  , _acStripeAPIKey :: SecretKey
  , _acRandomGenerator :: RandomGen
  }

type Runner m m' = forall a. m a -> m' a

data Runners m = Runners
  { _rAccountService :: Runner AccountService m
  , _rLog            :: Priority -> Text -> m ()
  , _rPublisher      :: Text -> Text -> ByteString -> m ()
  }

makeFields ''AppConfig
