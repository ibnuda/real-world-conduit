{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module API.Tags where

import           Protolude

import           Servant

import           Conf
import           Types

import           Coach.Tags

type TagsAPI = "tags" :> Get '[ JSON] ResponseTags

tagsApi :: MonadIO m => ServerT TagsAPI (CoachT m)
tagsApi = getTagsCoach

tagsProxy :: Proxy TagsAPI
tagsProxy = Proxy

tagsServer :: Configuration -> Server TagsAPI
tagsServer conf = hoistServer tagsProxy (coachToHandler conf) tagsApi
