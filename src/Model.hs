{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Lib.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Time
import           Database.Persist.Sql
import           Database.Persist.TH
import           Servant.Auth.Server

import Conf

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrateEverything"
  ]
  [persistLowerCase|
    User sql=users
      email Text
      username Text
      password Text
      bio Text Maybe
      image Text Maybe
      UniqueUsername username
      UniqueEmail email
      deriving Generic Show
    Follow sql=follows
      followerId UserId
      authorId UserId
      UniqueFollowerAuthor followerId authorId
      deriving Generic
    Article sql=articles
      slug Text
      authorId UserId
      title Text
      description Text
      body Text
      createdAt UTCTime
      updatedAt UTCTime Maybe
      UniqueSlug slug
      deriving Generic
    Tag sql=tags
      name Text
      UniqueName name
      deriving Generic
    Tagged
      articleId ArticleId
      tagId TagId
      UniqueArticleTag articleId tagId
      deriving Generic
    Favorited
      userId UserId
      articleId ArticleId
      UniqueUserArticle userId articleId
      deriving Generic
    Comment sql=comments
      body Text
      createdAt UTCTime
      updatedAt UTCTime Maybe
      articleId ArticleId
      userId UserId
      deriving Generic
  |]

data Auths (auths :: [*]) val

instance ToJSON User where
  toJSON = genericToJSON (aesonPrefix camelCase)
instance FromJSON User where
  parseJSON = genericParseJSON (aesonPrefix camelCase)
instance ToJWT User
instance FromJWT User

doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateEverything

runDb ::
     (MonadIO m, MonadReader Configuration m)
  => SqlPersistT IO b
  -> m b
runDb q = do
  pool <- asks configurationPool
  liftIO $ runSqlPool q pool
