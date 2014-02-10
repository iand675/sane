{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Logger where
import Control.Monad
import Control.Monad.Free
import Data.ByteString.Lazy (fromChunks)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStr, putStrLn)
import Network.AMQP
import Prelude (($), (>=), Ord, Eq, Enum)
import System.Console.ANSI
import System.IO (IO)

import qualified Sane.Messaging.Routes as M

data Priority = Debug | Info | Notice | Warning | Error | Critical | Alert | Emergency
  deriving (Ord, Eq, Enum)

data LoggerF cont = Log Priority Text cont
  deriving (Functor)

type Logger = Free LoggerF

log :: Priority -> Text -> Logger ()
log p t = liftF $ Log p t ()

stdoutLogger :: Priority -> Priority -> Text -> IO ()
stdoutLogger pMin p m = if p >= pMin
  then do
    putStr "["
    setSGR [SetColor Foreground Vivid priorityColor]
    putStr formattedPriority
    setSGR [Reset]
    putStrLn ("] - " <> m)
  else return ()
  where
    (priorityColor, formattedPriority) = case p of
      Debug     -> (Cyan, "DEBUG")
      Info      -> (Cyan, "INFO")
      Notice    -> (Blue, "NOTICE")
      Warning   -> (Yellow, "WARNING")
      Error     -> (Red, "ERROR")
      Critical  -> (Red, "CRITICAL")
      Alert     -> (Red, "ALERT")
      Emergency -> (Red, "EMERGENCY")

rabbitLogger :: Channel -> Priority -> Text -> IO ()
rabbitLogger c p msg = case M.topic M.LogExchange qTopic of
  Nothing -> return ()
  Just t  -> publishMsg c (M.exchange M.LogExchange) t $ newMsg { msgBody = fromChunks [encodeUtf8 msg] }
  where 
    qTopic = case p of
      Debug   -> M.Debug Proxy
      Info    -> M.Info Proxy
      Warning -> M.Warn Proxy
      Error   -> M.Error Proxy
      _       -> M.Warn Proxy
