{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ProtocolBuffers.Lens (
  ProtocolBuffer(..),
  field
) where
import Control.Lens
import Data.ProtocolBuffers hiding (field)

class ProtocolBuffer a pa | a -> pa, pa -> a where
  proto :: Iso' a pa

field :: HasField a => Iso' (FieldType a) a
field = iso putField getField
