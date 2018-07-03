{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conf where

import           Lib.Prelude

import           Database.Persist.Postgresql
import           Servant
import           Servant.Auth.Server

data Configuration = Configuration
  { configurationPool        :: ConnectionPool
  , configurationJWTSettings :: JWTSettings
  }

newtype CoachT m a = CoachT
  { runCoach :: ReaderT Configuration (ExceptT ServantErr m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Configuration
             , MonadError ServantErr
             , MonadIO
             )

type Coach = CoachT IO

coachToHandler :: Configuration -> Coach a -> Handler a
coachToHandler conf coach = Handler (runReaderT (runCoach coach) conf)
