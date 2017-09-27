{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Log.Backend.StandardOutput
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Data.Default (def)
import Data.Pool (Pool, createPool)
import Database.Bolt (Pipe, BoltCfg, user, password, connect, close)

import Api (app)

defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4j"}

run_server :: IO ()
run_server = do
  pool <- (createPool (connect defaultConfig) close 4 500 1)
  run  8080 $ app pool simpleStdoutLogger
