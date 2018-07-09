{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Coach.Tags where

import           Protolude

import           Database.Esqueleto

import           Conf
import           Model
import           Types

import           Que.Tags

getTagsCoach :: MonadIO m => CoachT m ResponseTags
getTagsCoach = do
  tags <- runDb selectTags
  return (resptags tags)
  where resptags xs = ResponseTags $ map (tagName . entityVal) xs
