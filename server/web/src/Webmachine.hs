module Webmachine where
import           Control.Monad.RWS

type Body = ()
type Status = ()
type Headers = ()

data WebmachineContext a = WebmachineContext
  { contextRequest :: Request
  , contextConfig :: a
  }

data WebmachineState s = WebmachineState
  { stateBody :: Body
  , stateStatus :: Status
  , stateHeaders :: Headers
  , stateUserState :: s
  }

type Webmachine c s = RWST (Context c) () s IO

