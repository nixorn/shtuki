{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Config where

import           Data.Text                            (pack)
import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (MonadLogger (..),
                                                       toLogStr)
import           Control.Monad.Metrics                (Metrics, MonadMetrics,
                                                       getMetrics)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, ask, asks)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)


import           Data.Pool                            (Pool, createPool)
import           Data.Default                         (def)
import           Database.Bolt                        (BoltCfg,
                                                       Pipe,
                                                       run,
                                                       password,
                                                       user,
                                                       host,
                                                       query_,
                                                       connect,
                                                       close)

import           Logger


type ConnectionPool = Pool Pipe

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServantErr m) a
    } deriving ( Functor, Applicative, Monad, MonadReader Config,
                 MonadError ServantErr, MonadIO)



type App = AppT IO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config
    = Config
    { configPool    :: ConnectionPool
    , configEnv     :: Environment
    , configMetrics :: Metrics
    , configLogEnv  :: LogEnv
    }

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks configLogEnv
    localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Production
    -- | Test
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
-- setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
    -- todo: log proper request data
    logMsg "web" InfoS "todo: received some request"
    liftIO $ app req respond


defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4j", host="neo"}


-- TODO: insert logger here
makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Production env = do
    pool <- runMaybeT $ do
        let timeout = 500
            resPerStripe = 1
        host <- (MaybeT . lookupEnv) "NEOHOST"
        user <- (MaybeT . lookupEnv) "NEOUSER"
        pass <- (MaybeT . lookupEnv) "NEOPASSWORD"
        lift $ createPool (connect $ def {user = pack user,
                                          password = pack pass,
                                          host = host})
                          close
                          (envPool Production)
                          timeout
                          resPerStripe
    case pool of
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just a -> return a
makePool Development env = makePool Production env

envPool :: Environment -> Int
-- envPool Test        = 1
envPool Development = 1
envPool Production  = 8
