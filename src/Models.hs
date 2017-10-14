{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Models where

import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, lift)
import           Data.Aeson           (FromJSON(..), ToJSON(..), withObject, object, (.=), (.:))
import           GHC.Generics         (Generic)
import           Database.Bolt        (Value (..), Record, RecordValue (..), Node (..), at, BoltActionT, run, queryP, query)
import           Data.Pool            (withResource)
import           Config               (Config, configPool)
import           Data.Text            (Text)
import           Data.Map             (Map)


data User = User {
    name  :: Text,
    email :: Text
  } deriving (Show, Eq)

instance ToJSON User where
  toJSON (User n e) = object ["name" .= n,
                              "email" .= e
                               ]

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
                                        <$> v .: "name"
                                        <*> v .: "email"


runQry :: (MonadReader Config m, MonadIO m) => Text -> m [Record]
runQry qry = do
    pool <- asks configPool
    liftIO $ withResource pool (`run` (query qry))


runQryParams :: (MonadReader Config m, MonadIO m) => Text -> Map Text Value -> m [Record]
runQryParams qry params = do
    pool <- asks configPool
    liftIO $ withResource pool (`run` (queryP qry params))

toUser :: (Monad m) => Record -> m User
toUser _rec = do
             T name <- _rec `at` "name"
             T email <- _rec `at` "email"
             return $ User name email


