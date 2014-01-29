{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Sane.Web.Types where
import Control.Lens.TH
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Sane.Models
import Sane.Common

data Expandable a = OnlyId (Id a) | Full a
data Patch a = NoChange | Update a

type Reminder = ()

