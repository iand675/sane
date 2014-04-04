{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Sane.Data.Tasks where
import           Control.Lens.TH
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Pool
import           Data.Metrics.Meter
import           Data.Metrics.Registry (register)
import           Data.Metrics.Timer
import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Utility
import           Prelude (($!), undefined)

import qualified Data.Cache as Cache
import           Sane.Common hiding (logger)
import qualified Sane.Common as C
import           Sane.Wire (encode, proto)
import qualified Sane.Wire as W
import           Sane.Data.Services hiding (createList, getLists, getList)
import           Sane.Data.Types hiding (fields, f)
import qualified Sane.Models.Accounts as D
import qualified Sane.Models.Common as D
import qualified Sane.Models.Lists as D
-- import qualified Sane.Models.Tasks as D
import           System.Logger

data TaskServiceMetrics = TaskServiceMetrics
  { _tsmListCreationTimer :: Timer IO
  , _tsmListCreationMeter :: Meter IO
  , _tsmGetListsTimer     :: Timer IO
  , _tsmGetListsMeter     :: Meter IO
  }

makeFields ''TaskServiceMetrics

data TaskServiceDependencies = TaskServiceDependencies
  { _tsLogger         :: Priority -> Text -> IO ()
  , _tsConnectionPool :: Pool Connection
  , _tsMetrics        :: TaskServiceMetrics
  , _tsCacheService   :: Runner Cache.CacheService IO
  }

makeFields ''TaskServiceDependencies

taskServiceDependencies :: MonadIO m => Action m AppConfig TaskServiceDependencies
taskServiceDependencies = act $ \conf -> do
  let r = conf ^. metricRegistry
  (Just clm) <- liftIO $ register r "list_create" meter
  (Just llm) <- liftIO $ register r "list_lists" meter
  return $ TaskServiceDependencies
    { _tsLogger = conf ^. C.logger
    , _tsConnectionPool = conf ^. taskConnectionPool
    , _tsMetrics = TaskServiceMetrics undefined clm undefined llm
    , _tsCacheService = Runner Cache.nullCacheService
    }

newtype TaskServiceT m a = TaskServiceT
  { fromTaskService :: ReaderT TaskServiceDependencies m a
  } deriving (Functor, Monad, Applicative, MonadIO)

type TaskServiceM = TaskServiceT IO

instance Monad m => MonadReader TaskServiceDependencies (TaskServiceT m) where
  ask = TaskServiceT ask
  local f m = TaskServiceT (local f $ fromTaskService m)

instance MonadTrans TaskServiceT where
  lift f = TaskServiceT $ lift f

instance Cache.Cacheable W.List Int

listKey :: D.Id D.List -> ByteString
listKey = ("list:" <>) . decimal

runTaskService :: MonadIO m => TaskServiceDependencies -> TaskService a -> m a
runTaskService deps m = liftIO $ (runReaderT . fromTaskService . iterM runTaskServiceImpl) m deps

runTaskServiceImpl :: MonadIO m => TaskServiceF (TaskServiceT m a) -> TaskServiceT m a
runTaskServiceImpl p = case p of
  (CreateList uid l n) -> createList uid l >>= n
  (GetLists uid n)     -> getLists uid >>= n
  (GetList lid n)      -> getList lid >>= n
  (UpdateList _ n)     -> n
  (CreateTask n)       -> n
  (GetTasks n)         -> n
  (UpdateTask n)       -> n

createList :: MonadIO m => D.Id D.User -> D.List -> TaskServiceT m (D.Persisted D.List)
createList uid l = do
  ms <- view metrics
  cs <- view cacheService
  liftIO $ ms ^! listCreationMeter . act mark
  logM <- view logger
  liftIO $ logM Debug "TaskServiceM - createList"
  transactionally $ \conn -> do
    let newList = l ^. dbList
    (Just (Only listId)) <- one $ query conn "insert into lists (owner, title, icon) values (?, ?, ?) returning id" (Only (D.fromId uid) :. newList)
    let pl = D.Persisted (D.Id listId) l
    cs ^. to run $ pl ^. proto . to encode . to (Cache.set (listKey $ D.Id listId))
    return pl

persistedList :: (Only (Id List) :. List) -> D.Persisted D.List
persistedList ((Only lid) :. l) = D.Persisted (D.Id lid) $ D.List
  { D._listTitle    = l ^. title
  , D._listIcon     = l ^. icon
  , D._listArchived = l ^. archived
  , D._listOwner    = l ^. owner . to D.Id
  }

getLists :: MonadIO m => D.Id D.User -> TaskServiceT m [D.Persisted D.List]
getLists uid = do
  ms <- view metrics
  liftIO $ ms ^! getListsMeter . act mark
  logM <- view logger
  liftIO $ logM Debug "TaskServiceM - getLists"
  transactionally $ \conn -> do
    ls <- query conn "select id, title, icon, archived, owner from lists where owner = ?" $ Only $ D.fromId uid
    return $! fmap persistedList ls

getList :: MonadIO m => D.Id D.List -> TaskServiceT m (Maybe (D.Persisted D.List))
getList lid = do
  -- liftIO $ ms ^. getListMeter . act mark
  logM <- view logger
  cs <- view cacheService
  liftIO $ logM Debug "TaskServiceM - getList"
  mList <- liftIO $ (cs ^. to run) $ lid ^. to listKey . to Cache.get
  let getAndCacheList = withDb $ \conn -> do
        ml <- one $ query conn "select id, title, icon, archived, owner from lists where id = ? limit 1" $ Only $ D.fromId lid
        case ml of
          Nothing -> return Nothing
          Just l -> do
            let pl = persistedList l
            (cs ^. to run) $ pl ^. proto . to encode . to (Cache.set (listKey lid))
            return $ Just pl
  case mList of
    Nothing -> getAndCacheList
    Just l -> case Cache.cacheDecode l :: Maybe W.List of
      Nothing -> getAndCacheList
      Just lc -> return $ Just $ D.Persisted lid $ lc ^. from proto

withDb :: MonadIO m => (Connection -> IO a) -> TaskServiceT m a
withDb f = TaskServiceT $ do
  pool <- view connectionPool
  liftIO $ withResource pool f

transactionally :: MonadIO m => (Connection -> IO a) -> TaskServiceT m a
transactionally f = withDb $ \conn -> withTransaction conn (f conn)
