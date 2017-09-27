{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           Data.Map.Strict            (fromList, (!))
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool, createPool)
import           Data.Text                  (Text)
import           Database.Bolt

import Types

data ServerState = ServerState { pool :: Pool Pipe }

type WebM = ReaderT ServerState IO
