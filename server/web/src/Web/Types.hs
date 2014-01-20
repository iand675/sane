{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Types where
import Control.Lens.TH
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Domain.Types
import Common

data Expandable a = OnlyId (Id a) | Full a
data Patch a = NoChange | Update a

type Reminder = ()

deriveJSON defaultOptions ''NewUser
deriveJSON defaultOptions ''CurrentUser
deriveJSON defaultOptions ''FullUser
