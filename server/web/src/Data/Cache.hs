{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Cache where
import           Control.Lens hiding (set)
import           Control.Monad
import           Control.Monad.Free
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.ProtocolBuffers
import qualified Database.Redis as R
import qualified GHC.Generics as G
import           Prelude

data CacheServiceF c
  = Get ByteString (Maybe ByteString -> c)
  | Set ByteString ByteString c
  | Delete ByteString c
  deriving (Functor)

type CacheService = Free CacheServiceF

get :: ByteString -> CacheService (Maybe ByteString)
get k = liftF $ Get k id

set :: ByteString -> ByteString -> CacheService ()
set k v = liftF $ Set k v ()

delete :: ByteString -> CacheService ()
delete k = liftF $ Delete k ()

nullCacheService :: Monad m => CacheService a -> m a
nullCacheService = iterM runNullCacheService
  where
    runNullCacheService c = case c of
      (Get _ n)    -> n Nothing
      (Set _ _ n)  -> n
      (Delete _ n) -> n

redisCacheService :: R.Connection -> CacheService a -> IO a
redisCacheService conn = R.runRedis conn . iterM runRedisCacheService
  where
    runRedisCacheService :: CacheServiceF (R.Redis a) -> R.Redis a
    runRedisCacheService c = case c of
      (Get k n)    -> do
        emv <- R.get k
        case emv of
          Left _   -> n Nothing
          Right mv -> n mv
      (Set k v n)  -> R.set k v >> n
      (Delete k n) -> R.del [k] >> n

cache :: Cacheable a k => k -> a -> CacheService ()
cache k x = set (makeKey x k) $ cacheEncode x

getWithKey :: forall a k. Cacheable a k => k -> CacheService (Maybe a)
getWithKey k = do
  mbs <- get $ makeKey (undefined :: a) k
  return $ case mbs of
    Nothing -> Nothing
    Just bs -> cacheDecode bs

class Cacheable a k | a -> k where
  makeKey     :: a -> k -> ByteString
  default makeKey :: (G.Generic a, CacheKey k, G.Datatype d, G.Rep a ~ t d (f :: * -> *)) => a -> k -> ByteString
  makeKey x k = L.toStrict $ toLazyByteString (stringUtf8 dtn <> char8 ':' <> encodeKey k)
    where
      dtn = G.datatypeName gen
      gen = G.from x

  cacheEncode :: a -> ByteString
  default cacheEncode :: Encode a => a -> ByteString
  cacheEncode = runPut . encodeMessage

  cacheDecode :: ByteString -> Maybe a
  default cacheDecode :: Decode a => ByteString -> Maybe a
  cacheDecode b = runGet decodeMessage b ^? _Right

class CacheKey a where
  encodeKey :: a -> Builder

instance CacheKey Int where
  encodeKey = intDec

