{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import qualified Control.Monad.Metrics       as Metrics
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Data.Int                    (Int64)
import           Data.IORef                  (readIORef)
import           Data.Text                   (Text, pack)
import           Lens.Micro                  ((^.))
import           Network.Wai                 (Application)
import           Network.Wai.Metrics
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (AppT (..), Config (..))
import           Control.Monad.Metrics       (increment, metricsCounters)
import           Data.IORef                  (readIORef)
import           Data.Map                    (Map, fromList)
import           Data.Text                   (Text)
import           Lens.Micro                  ((^.))
import           Models                      (User (..), runQryParams, runQry, toUser)
import           Database.Bolt               (Value(..))
import qualified System.Metrics.Counter      as Counter

type UserAPI =
         "users" :> Get '[JSON] ([User])
    :<|> "users" :> Capture "name" Text :> Get '[JSON] ([User])
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] ([User])
    :<|> "metrics" :> Get '[JSON] (Map Text Int64)

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> usersByName :<|> createUser :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [User]
allUsers = do
    increment "allUsers"
    logDebugNS "web" "allUsers"
    let cypher = "MATCH (u:User) RETURN u;"
    records <- runQry cypher
    traverse toUser records

usersByName :: MonadIO m => Text -> AppT m [User]
usersByName name = do
    increment "singleUser"
    logDebugNS "web" "get user by name"
    let cypher = "MATCH (u:User {name:{name}}) RETURN u;"
        params = fromList [("name", T name)]
    users <- runQryParams cypher params
    traverse toUser users

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m [User]
createUser user = do
    increment "createUser"
    logDebugNS "web" "creating a user"
    let cypher = "CREATE (User {name:{name}, email:{email}})"
        params = fromList [("email", T $ email user), ("name", T $ name user)]
    records <- runQryParams cypher params
    traverse toUser records

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (Map Text Int64)
waiMetrics = do
    increment "metrics"
    logDebugNS "web" "metrics"
    metr <- Metrics.getMetrics
    liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)

-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"

