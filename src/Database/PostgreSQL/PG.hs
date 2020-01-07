{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.PostgreSQL.PG (
    PG,
    execute,
    execute_,
    inTransaction,
    query,
    query_,
    runPG,
    withConnectionString
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Connection, Query, FromRow, ToRow, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple as Postgres
import GHC.Int (Int64)

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

newtype PG a = PG { unPG :: ReaderT Connection IO a } deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO, MonadFail, Alternative)

runPG :: MonadIO m => PG a -> Connection -> m a
runPG (PG a) = liftIO . runReaderT a 

withConnectionString :: MonadIO m => ByteString -> PG a -> m a 
withConnectionString connectionString (PG a) = liftIO $ connectPostgreSQL connectionString >>= runReaderT a 

withConn1 :: (Connection -> a -> IO b) -> a -> PG b 
withConn1 f a = ask >>= liftIO . flip f a

withConn2 :: (Connection -> a -> b -> IO c) -> a -> b -> PG c
withConn2 f a b = ask >>= liftIO . flip3 f a b 

execute_ :: Query -> PG Int64 
execute_ = withConn1 Postgres.execute_

execute :: ToRow q => Query -> q -> PG Int64 
execute = withConn2 Postgres.execute 

query_ :: FromRow r => Query -> PG [r]
query_ = withConn1 Postgres.query_ 

query :: (ToRow q, FromRow r) => Query -> q -> PG [r]
query = withConn2 Postgres.query 

inTransaction :: PG a -> PG a
inTransaction (PG a) = do
    conn <- ask
    liftIO $ Postgres.withTransaction conn $ runReaderT a conn
