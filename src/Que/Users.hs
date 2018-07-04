{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Que.Users where

import           Lib.Prelude        hiding (from, get, on, (<&>))

import           Database.Esqueleto

import           Model
import           Util

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

selectUserByMaybeUsernameEmail ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Maybe Text
  -> Maybe Text
  -> ReaderT backend m [Entity User]
selectUserByMaybeUsernameEmail musername memail = do
  select $
    from $ \user -> do
      where_ (whereBuilderOr musername user UserUsername
              ||. whereBuilderOr memail user UserEmail)
      return user
  where
    whereBuilderOr Nothing _ _              = val False
    whereBuilderOr (Just x) entity accessor = entity ^. accessor ==. val x

insertUserEntity ::
     ( BaseBackend backend ~ SqlBackend
     , MonadIO m
     , PersistStoreWrite backend
     )
  => User
  -> ReaderT backend m (Entity User)
insertUserEntity user = insertEntity user

updateUser ::
     MonadIO m
  => Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ReaderT SqlBackend m ()
updateUser username memail musername mpassword mimage mbio = do
  mpassword' <- liftIO $ mapM generatePassword mpassword
  update $ \user -> do
    set
      user
      [ updateByMaybe memail user UserEmail
      , updateByMaybe musername user UserUsername
      , updateByMaybe mpassword' user UserPassword
      , UserImage =. val mimage
      , UserBio =. val mbio
      ]
    where_ (user ^. UserUsername ==. val username)
  where
    updateByMaybe (Just x) _ accessor     = accessor =. val x
    updateByMaybe Nothing entity accessor = accessor =. entity ^. accessor
