{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module RealWorld where

import           Lib.Prelude

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.Warp    (Port)
import           Servant
import           Servant.Auth
import           Servant.Auth.Server

import           Conf
import           Model

import           API.User

type ConduitAPI auth =
  "api" :> (Servant.Auth.Server.Auth auth User :> UserInformationAPI)
   :<|> "api" :> UserAdministrationAPI
   :<|> Raw

conduitProxy :: Proxy (ConduitAPI '[JWT])
conduitProxy = Proxy

conduitServer :: Configuration -> Server (ConduitAPI auth)
conduitServer conf =
  userInformationServer conf
  :<|> userAdministrationServer conf
  :<|> serveDirectoryFileServer "front"

connstring :: ByteString
connstring =
  "host=localhost "
  <> "port=5432 "
  <> "user=ibnu "
  <> "password=jaran "
  <> "dbname=uwu"

running :: IO ()
running = do
  jwk <- generateKey
  pool <- runStderrLoggingT (createPostgresqlPool connstring 10)
  let jws = defaultJWTSettings jwk
      cfg = defaultCookieSettings :. jws :. EmptyContext
      conf = Configuration pool jws
  runSqlPool doMigration pool
  run 8080 (serveWithContext conduitProxy cfg (conduitServer conf))

stop :: IO ()
stop = return ()

startServe :: IO (Port, Application)
startServe = do
  jwk <- generateKey
  pool <- runStderrLoggingT (createPostgresqlPool connstring 10)
  let jws = defaultJWTSettings jwk
      cfg = defaultCookieSettings :. jws :. EmptyContext
      conf = Configuration pool jws
  runSqlPool doMigration pool
  return (8080, serveWithContext conduitProxy cfg (conduitServer conf))
