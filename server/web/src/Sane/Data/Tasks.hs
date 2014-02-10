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
import           Data.Pool
import           Data.Metrics.Meter
import           Data.Metrics.Registry (register)
import           Data.Metrics.Timer
import           Data.Text (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Utility
import           Prelude (($!), undefined)

import           Sane.Common
import           Sane.Data.Services hiding (createList, getLists)
import           Sane.Data.Types hiding (fields, f)
import qualified Sane.Models as D
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
  }

makeFields ''TaskServiceDependencies

taskServiceDependencies :: MonadIO m => Action m AppConfig TaskServiceDependencies
taskServiceDependencies = act $ \conf -> do
  let r = conf ^. metricRegistry
  (Just clm) <- liftIO $ register r "list_create" meter
  (Just llm) <- liftIO $ register r "list_lists" meter
  return $ TaskServiceDependencies
    { _tsLogger = stdoutLogger $ conf ^. minLogLevel
    , _tsConnectionPool = conf ^. taskConnectionPool
    , _tsMetrics = TaskServiceMetrics undefined clm undefined llm
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

runTaskService :: MonadIO m => TaskServiceDependencies -> TaskService a -> m a
runTaskService deps m = liftIO $ (runReaderT . fromTaskService . iterM runTaskServiceImpl) m deps

runTaskServiceImpl :: MonadIO m => TaskServiceF (TaskServiceT m a) -> TaskServiceT m a
runTaskServiceImpl p = case p of
  (CreateList uid l n) -> createList uid l >>= n
  (GetLists uid n)     -> getLists uid >>= n
  (CreateTask n)       -> n
  (GetTasks n)         -> n
  (UpdateTask n)       -> n

createList :: MonadIO m => D.Id D.User -> D.List -> TaskServiceT m (D.Persisted D.List)
createList uid l = do
  ms <- view metrics
  liftIO $ ms ^! listCreationMeter . act mark
  logM <- view logger
  transactionally $ \conn -> do
    logM Debug "TaskServiceM - createList"
    let newList = l ^. dbList
    (Just (Only listId)) <- one $ query conn "insert into lists (owner, title, icon) values (?, ?, ?) returning id" (Only uid :. newList)
    return $! D.Persisted listId l

persistedList :: (Only (Id List) :. List) -> D.Persisted D.List
persistedList ((Only lid) :. l) = D.Persisted lid $ D.List
  { D._listTitle = l ^. title
  , D._listIcon  = l ^. icon
  }

getLists :: MonadIO m => Id User -> TaskServiceT m [D.Persisted D.List]
getLists uid = do
  ms <- view metrics
  liftIO $ ms ^! getListsMeter . act mark
  logM <- view logger
  transactionally $ \conn -> do
    logM Debug "TaskServiceM - getLists"
    ls <- query conn "select id, title, icon from lists where owner = ?" $ Only uid
    return $! fmap persistedList ls

withDb :: MonadIO m => (Connection -> IO a) -> TaskServiceT m a
withDb f = TaskServiceT $ do
  pool <- view connectionPool
  liftIO $ withResource pool f

transactionally :: MonadIO m => (Connection -> IO a) -> TaskServiceT m a
transactionally f = withDb $ \conn -> withTransaction conn (f conn)
