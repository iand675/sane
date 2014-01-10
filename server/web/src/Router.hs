{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Router where
import           Data.Text (Text)
import           Prelude hiding ((.), id)
import           Control.Category ((.), id)
import           Text.Boomerang
import           Text.Boomerang.Texts
import           Text.Boomerang.TH

type Router a = Boomerang TextsError [Text] () (a :- ())

data SaneAction
  = SignIn
  | SignOut
  deriving (Eq, Show)

makeBoomerangs ''SaneAction

saneAction :: Router SaneAction
saneAction = (  rSignIn . "signin"
             <> rSignOut . "signout"
             )

parseRoute = parseTexts saneAction
