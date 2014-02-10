module Main where
import Test.Hspec
import Data.Accounts

main :: IO ()
main = do
  s <- createUserSpec
  u <- getUserSpec
  si <- signInSpec
  hspec $ do
    s
    u
    si
