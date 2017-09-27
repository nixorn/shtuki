{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where
import           Data.Aeson (
  FromJSON (..), ToJSON (..), withObject, object, (.=), (.:))
import           Data.Text    (Text)
import           Data.Map     (Map)

import Database.Bolt (Record, Value (..), RecordValue (..), Node (..), at)


data Lot = Lot {
  _id      :: Int
  , _name  :: Text
  , _desc  :: Text
  , _price :: Int
  } deriving (Show, Eq)


data Category = Category {
  _id            :: Int
  , _name        :: Text
  } deriving (Show, Eq)


data User = User {
  _id           :: Int
  , _token      :: Text
  , _vk_id      :: Text
  , _first_name :: Text
  , _last_name  :: Text
  } deriving (Show, Eq)

data MNode = MNode {
  _mnTitle  :: Text
  , _label  :: Text
  } deriving (Show, Eq, Ord)

data MRel = MRel {
  _source   :: Int
  , _target :: Int
  } deriving (Show, Eq)

data MGraph = MGraph {
  _nodes   :: [MNode]
  , _links :: [MRel]
  } deriving (Show, Eq)

instance ToJSON Value where
  toJSON (N _) = toJSON ()
  toJSON (B b) = toJSON b
  toJSON (I i) = toJSON i
  toJSON (F d) = toJSON d
  toJSON (T t) = toJSON t
  toJSON (L l) = toJSON l
  toJSON _ = undefined

data Node = Node {
  nodeIdentity :: Int
  , labels       :: [Text]
  , nodeProps    :: Map Text Value
  } deriving (Show, Eq)

data Relationship = Relationship {
  relIdentity   :: Int
  , startNodeId :: Int
  , endNodeId   :: Int
  , relType     :: Text
  , relProps    :: Map Text Value
  } deriving (Show, Eq)


instance ToJSON Lot where
  toJSON (Lot i n d p) = object ["id" .= i, "name" .= n
                                , "desc" .= d, "price" .= p
                                ]


instance ToJSON User where
  toJSON (User i t v f l) = object ["id" .= i,
                                   "token" .= t,
                                   "vk_id" .= v,
                                   "first_name" .= f,
                                   "last_name" .= l
                                    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
                                        <$> v .: "id"
                                        <*> v .: "token"
                                        <*> v .: "vk_id"
                                        <*> v .: "first_name"
                                        <*> v .: "last_name"


instance ToJSON Category where
  toJSON (Category i n) = object [
    "id" .= i, "name" .= n
    ]


instance ToJSON MNode where
  toJSON (MNode t l) = object ["title" .= t, "label" .= l]

instance ToJSON MRel where
  toJSON (MRel s t) = object ["source" .= s, "target" .= t]

instance ToJSON MGraph where
  toJSON (MGraph n r) = object ["nodes" .= n, "links" .= r]


