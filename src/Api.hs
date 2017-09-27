{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Api where

import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Database.Bolt (Pipe)
import Servant
import Log
import qualified Control.Category

import Types

type UserID = Text


instance MonadTime Handler where
    currentTime = liftIO currentTime

class MonadDB m where
    withConnection :: (Pipe -> m a) -> m a

newtype H a = H { runH :: ReaderT (Pool Pipe) (LogT Handler) a }
   deriving (Functor, Applicative, Monad, MonadTime, MonadLog)

instance MonadDB H where
    withConnection f = H $ do
        pool <- ask
        withResource pool $ \conn -> runH (f conn)


createUser :: (MonadDB m, MonadLog m) => User -> m User
createUser = error "not implemented"

readUser :: (MonadDB m) => Integer -> m User
readUser = error "not implemented"


type UserAPI = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
               :<|> "users" :> Capture "id" Integer :> Get '[JSON] User


userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Pool Pipe -> Logger -> Application
app pool logger = serve userAPI $ enter nt $ createUser :<|> readUser
  where
    nt = NT $ \m -> runLogT "api" logger (runReaderT (runH m) pool)
