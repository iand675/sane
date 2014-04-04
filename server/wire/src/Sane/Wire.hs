module Sane.Wire (
  module Data.ProtocolBuffers.Lens,
  module Sane.Wire.Accounts,
  module Sane.Wire.Common,
  module Sane.Wire.Lists,
  module Sane.Wire.Tasks,
  encode
  ) where
import Data.ByteString (ByteString)
import Data.ProtocolBuffers (encodeMessage, Encode)
import Data.ProtocolBuffers.Lens
import qualified Data.Serialize.Put as S
import Sane.Wire.Accounts
import Sane.Wire.Common
import Sane.Wire.Lists
import Sane.Wire.Tasks

encode :: Encode a => a -> ByteString
encode = S.runPut . encodeMessage
