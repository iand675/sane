{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common (
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
  stripeAPIKey
) where
import Control.Applicative
import Control.Category
import Control.Lens
import Control.Lens.TH (makeFields)
import Control.Monad
import Data.Bool
import Data.Either
import Data.Eq
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Data.Void
import qualified System.IO as IO
import Text.Show
import qualified Prelude as P
import Web.Stripe.Client (APIKey)

data AppConfig = AppConfig
  { _acAccountConnectionPool :: Pool Connection
  , _acStripeAPIKey :: APIKey
  }

makeFields ''AppConfig
