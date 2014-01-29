{-# LANGUAGE DeriveFunctor #-}
module System.Logger where
import Control.Monad.Free
import Data.Functor
import Data.Text (Text)
import Prelude (($))

data Priority = Debug | Info | Notice | Warning | Error | Critical | Alert | Emergency

data LoggerF cont = Log Priority Text cont
  deriving (Functor)

type Logger = Free LoggerF

log :: Priority -> Text -> Logger ()
log p t = liftF $ Log p t ()

