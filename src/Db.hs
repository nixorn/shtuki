{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Pool                  (Pool)
import           Database.Bolt              (Pipe)

import Types

data ServerState = ServerState { pool :: Pool Pipe }

type WebM = ReaderT ServerState IO
