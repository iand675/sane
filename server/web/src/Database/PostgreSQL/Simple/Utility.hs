{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.Simple.Utility where
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Typeable
import Prelude (($), Show)
import System.IO

data TooManyResultsException = TooManyResultsException
  deriving (Typeable, Show)

instance Exception TooManyResultsException

one :: IO [a] -> IO (Maybe a)
one m = do
  l <- m
  case l of
    [] -> return $ Nothing
    (x:[]) -> return $ Just x
    _ -> throw TooManyResultsException

