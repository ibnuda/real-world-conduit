{-# LANGUAGE FlexibleContexts      #-}
module Que.Tags where

import           Lib.Prelude          hiding (from, get, on, (<&>))

import           Database.Esqueleto

import           Model

selectTags ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [Entity Tag]
selectTags =
  select $
    from $ \tag -> do
      orderBy [asc (tag ^. TagName)]
      return tag
