{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sane.Models.Lists where
import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Data.Text (Text)
import           Data.ProtocolBuffers.Lens
import           Network.HTTP.JSON.API
import           Prelude (Maybe, Bool, Eq, Show, ($), (.), const, fromIntegral)
import           Sane.Models.Accounts
import           Sane.Models.Common
import           Sane.Models.JSON
import           Sane.Routes hiding (Username, Id, fromId)
import qualified Sane.Wire.Lists as W

data List = List
  { _listTitle    :: Text
  , _listIcon     :: Maybe Text
  , _listArchived :: Bool
  , _listOwner    :: Id User
  } deriving (Eq, Show)

makeFields ''List
jsonize ''List

instance JsonAPIDocument List where
  documentType = to $ const "list"

instance ProtocolBuffer List W.List where
  proto = iso toWire fromWire
    where
      toWire = W.List
        <$> view (title . field)
        <*> view (icon . field)
        <*> view (archived . field)
        <*> view (owner . to (fromIntegral . fromId) . field)
      fromWire = List
        <$> ((view $ from field) . W.listName)
        <*> ((view $ from field) . W.listIcon)
        <*> ((view $ from field) . W.listArchived)
        <*> ((view $ from field . to (Id . fromIntegral)) . W.listOwner)
