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

import Network.Wai.Handler.Warp
import Log.Backend.StandardOutput (simpleStdoutLogger)


import Data.Default (def)
import Data.Pool (createPool)
import Database.Bolt (BoltCfg, user, password, connect, close)

import Api (app)

defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4j"}

run_server :: IO ()
run_server = do
  pool <- (createPool (connect defaultConfig) close 4 500 1)
  run 8080 $ app pool simpleStdoutLogger
