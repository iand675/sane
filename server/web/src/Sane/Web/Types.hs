{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Sane.Web.Types where
import Sane.Models

data Expandable a = OnlyId (Id a) | Full a
data Patch a = NoChange | Update a

type Reminder = ()

