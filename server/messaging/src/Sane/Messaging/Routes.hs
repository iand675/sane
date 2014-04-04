{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sane.Messaging.Routes where
import Control.Category ((.))
import Data.Proxy
import Data.Text (Text, intercalate, split)
import Text.Boomerang
import Text.Boomerang.Texts
import Text.Boomerang.TH
import Prelude (fmap, Maybe, Either, (==), Show)

class Exchange e t | e -> t where
  topic :: e -> t -> Maybe Text
  exchange :: e -> Text
  fromTopic :: e -> Text -> Either TextsError t

data AccountExchange = AccountExchange

type Username = Text
data UserMessage
  = UserCreated (Proxy Username)
  | PasswordReset (Proxy Username)
  deriving (Show)

makeBoomerangs ''UserMessage

type LogInfo = Text

data LogExchange = LogExchange
data LogMessage
  = Debug      (Proxy LogInfo)
  | Info       (Proxy LogInfo)
  | Warn       (Proxy LogInfo)
  | Error      (Proxy LogInfo)
  | Other Text (Proxy LogInfo)
  deriving (Show)

makeBoomerangs ''LogMessage

payload :: Boomerang TextsError [Text] a (Proxy b :- a)
payload = push Proxy

accountTopics :: Boomerang TextsError [Text] a (UserMessage :- a)
accountTopics = "user" </>
  (  (rUserCreated   . "created"        . payload)
  <> (rPasswordReset . "password_reset" . payload)
  )

logTopics :: Boomerang TextsError [Text] a (LogMessage :- a)
logTopics =
  (  (rDebug . "debug" . payload)
  <> (rInfo  . "info"  . payload)
  <> (rWarn  . "warn"  . payload)
  <> (rError . "error" . payload)
  <> (rOther . anyText . payload)
  )

unparseTopic :: Boomerang TextsError [Text] () (a :- ()) -> a -> Maybe Text
unparseTopic b = fmap (intercalate ".") . unparseTexts b

parseTopic :: Boomerang TextsError [Text] () (a :- ()) -> Text -> Either TextsError a
parseTopic b = parseTexts b . split (== '.')

instance Exchange AccountExchange UserMessage where
  topic _ = unparseTopic accountTopics
  exchange _ = "account"
  fromTopic _ = parseTopic accountTopics

instance Exchange LogExchange LogMessage where
  topic _ = unparseTopic logTopics
  exchange _ = "log"
  fromTopic _ = parseTopic logTopics
