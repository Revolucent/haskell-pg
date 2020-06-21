{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.PG (
    PG,
    bracketWithConnection,
    bracketWithConnectionString,
    execute,
    execute_,
    getConnection,
    query,
    query_,
    query1_,
    runPG,
    withConnection,
    withConnectionString,
    withTransaction
) where

import Prelude hiding (head)

import Control.Applicative (Alternative, Applicative)
import Data.List.Safe (head)
import Control.Monad (Monad)
import Control.Monad.Catch (bracket, MonadCatch, MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, ReaderT(..))
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Connection, Query, FromRow, ToRow, connectPostgreSQL, close, fromOnly, Only(..))
import qualified Database.PostgreSQL.Simple as Postgres
import GHC.Int (Int64)

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

newtype PG a = PG { unPG :: ReaderT Connection IO a } deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadFail, Alternative)

getConnection :: PG Connection
getConnection = PG ask

runPG :: MonadIO m => PG a -> Connection -> m a
runPG (PG a) = liftIO . runReaderT a

withConnection :: MonadIO m => Connection -> PG a -> m a
withConnection conn a = runPG a conn

withConnectionString :: MonadIO m => ByteString -> PG a -> m a 
withConnectionString connectionString (PG a) = liftIO $ connectPostgreSQL connectionString >>= runReaderT a 

bracketWithConnection :: Connection -> (Connection -> IO a) -> IO a
bracketWithConnection conn = bracket (return conn) close 

bracketWithConnectionString :: ByteString -> (Connection -> IO a) -> IO a
bracketWithConnectionString connectionString = bracket (connectPostgreSQL connectionString) close 

withConn1 :: (Connection -> a -> IO b) -> a -> PG b 
withConn1 f a = getConnection >>= liftIO . flip f a

withConn2 :: (Connection -> a -> b -> IO c) -> a -> b -> PG c
withConn2 f a b = getConnection >>= liftIO . flip3 f a b 

execute_ :: Query -> PG Int64 
execute_ = withConn1 Postgres.execute_

execute :: ToRow q => Query -> q -> PG Int64 
execute = withConn2 Postgres.execute 

query_ :: FromRow r => Query -> PG [r]
query_ = withConn1 Postgres.query_ 

query :: (ToRow q, FromRow r) => Query -> q -> PG [r]
query = withConn2 Postgres.query 

query1_ :: FromRow r => Query -> PG r 
query1_ sql = query_ sql >>= head

query1 :: (ToRow q, FromRow r) => Query -> q -> PG r 
query1 sql q = query sql q >>= head 

withTransaction :: PG a -> PG a
withTransaction (PG a) = do
    conn <- getConnection 
    liftIO $ Postgres.withTransaction conn $ runReaderT a conn
