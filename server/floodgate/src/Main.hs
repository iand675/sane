{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Trans
import           Data.Monoid
import           Data.Text.Lazy (Text, pack, fromStrict)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO as T
import           Network.AMQP
import           Pipes
import qualified Pipes.Concurrent as C
import           Pipes.AMQP
import qualified Pipes.Prelude as P
import           System.Console.ANSI
import           System.Posix.Signals

import           Sane.Messaging.Routes

formatLogOutput :: LogMessage -> (Color, Text)
formatLogOutput p = case p of
  Debug _   -> (Cyan, "DEBUG")
  Info  _   -> (Cyan, "INFO")
  Warn  _   -> (Yellow, "WARNING")
  Error _   -> (Red, "ERROR")
  Other t _ -> (Blue, fromStrict t)

parseLog :: Monad m => Pipe (Message, Envelope) (LogMessage, Text) m r
parseLog = do
  (m, e) <- await
  case parseTopic logTopics (envRoutingKey e) of
    Left _ -> parseLog
    Right t -> do
      yield (t, decodeUtf8 $ msgBody m)
      parseLog

printLog :: Consumer' (LogMessage, Text) IO ()
printLog = do
  (m, t) <- await
  let (color, level) = formatLogOutput m
  let setColor = pack $ setSGRCode [SetColor Foreground Vivid color]
  let reset = pack $ setSGRCode [Reset]
  liftIO $ T.putStrLn ("[" <> setColor <> level <> reset <> "] - " <> t)
  printLog

main = do
  conn <- openConnection "192.168.33.10" "/" "rabbit_whisperer" "R4bbit!"
  chan <- openChannel conn
  (name, _, _) <- declareQueue chan newQueue
  bindQueue chan name "log" "*"
  (receiver, worker) <- receiveMessages chan NoAck C.Single name
  result <- Async.async $ runEffect (worker >-> parseLog >-> printLog)
  installHandler killProcess (Catch $ terminateReceiver receiver) Nothing
  Async.wait result
