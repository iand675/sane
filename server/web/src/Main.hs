{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as C
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

import Router

main :: IO ()
main = do
  Warp.run 3000 $ \request -> do
    case parseRoute $ pathInfo request of
      Left err -> return $ responseLBS notFound404 [] "unrecognized resource"
      Right action -> handleAction action

handleAction action = case action of
  SignIn ->
  SignOut ->
