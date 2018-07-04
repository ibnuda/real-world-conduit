{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Que.Users where

import           Lib.Prelude          hiding (from, get, on, (<&>))

import           Database.Esqueleto

import           Model

selectUserByUsernameEmail ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> Text
  -> ReaderT backend m [Entity User]
selectUserByUsernameEmail username email = do
  select $
    from $ \user -> do
      where_
        (user ^. UserUsername ==. val username
         ||. user ^. UserEmail ==. val email)
      return user

insertUserEntity ::
     ( BaseBackend backend ~ SqlBackend
     , MonadIO m
     , PersistStoreWrite backend
     )
  => User
  -> ReaderT backend m (Entity User)
insertUserEntity user = insertEntity user
