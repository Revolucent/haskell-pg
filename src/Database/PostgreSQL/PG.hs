{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.PG (
    execute,
    execute_,
    liftPG,
    query,
    query_,
    query1,
    query1_,
    value1,
    value1_,
    runPG,
    withTransaction,
    PG
) where

import Prelude hiding (head)

import Data.List.Safe (head)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, MonadReader, ReaderT(..))
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Connection, Query, FromRow, ToRow, fromOnly, connectPostgreSQL)
import Database.PostgreSQL.Simple.FromField (FromField)
import qualified Database.PostgreSQL.Simple as Postgres
import GHC.Int (Int64)

type PG = ReaderT Connection IO

runPG :: MonadIO m => PG a -> Connection -> m a
runPG a = liftIO . runReaderT a

liftPG :: (MonadIO m, MonadReader Connection m) => PG a -> m a
liftPG a = ask >>= runPG a

withTransaction :: (MonadIO m, MonadReader Connection m) => PG a -> m a
withTransaction a = do 
    conn <- ask
    liftIO $ Postgres.withTransaction conn $ runPG a conn

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

withConn1 :: (MonadReader Connection m, MonadIO m) => (Connection -> a -> IO b) -> a -> m b 
withConn1 f a = ask >>= liftIO . flip f a

withConn2 :: (MonadReader Connection m, MonadIO m) => (Connection -> a -> b -> IO c) -> a -> b -> m c
withConn2 f a b = ask >>= liftIO . flip3 f a b 

execute_ :: (MonadReader Connection m, MonadIO m) => Query -> m Int64 
execute_ = withConn1 Postgres.execute_

execute :: (MonadReader Connection m, MonadIO m) => ToRow q => Query -> q -> m Int64 
execute = withConn2 Postgres.execute 

query_ :: (MonadReader Connection m, MonadIO m) => FromRow r => Query -> m [r]
query_ = withConn1 Postgres.query_ 

query1_ :: (MonadReader Connection m, MonadIO m, MonadThrow m) => FromRow r => Query -> m r 
query1_ sql = query_ sql >>= head

value1_ :: (MonadReader Connection m, MonadIO m, MonadThrow m, FromField a) => Query -> m a
value1_ sql = fromOnly <$> query1_ sql

query :: (MonadReader Connection m, MonadIO m) => (ToRow q, FromRow r) => Query -> q -> m [r]
query = withConn2 Postgres.query 

query1 :: (MonadReader Connection m, MonadIO m, MonadThrow m) => (ToRow q, FromRow r) => Query -> q -> m r 
query1 sql q = query sql q >>= head

value1 :: (MonadReader Connection m, MonadIO m, MonadThrow m, ToRow q, FromField a) => Query -> q -> m a
value1 sql q = fromOnly <$> query1 sql q
