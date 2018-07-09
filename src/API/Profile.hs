{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module API.Profile where

import           Lib.Prelude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Coach.Profile

type UserProfileAPI =
  "profiles"
    :> Capture "username" Text
    :> Get '[ JSON] ResponseProfile
  :<|> "profiles"
    :> Capture "username" Text
    :> "follow"
    :> PostNoContent '[ JSON] NoContent
  :<|> "profiles"
    :> Capture "username" Text
    :> "follow"
    :> DeleteNoContent '[ JSON] NoContent

userProfileApi
  :: MonadIO m => AuthResult User -> ServerT UserProfileAPI (CoachT m)
userProfileApi authres =
  getUserProfileCoach authres
    :<|> postUserFollowCoach authres
    :<|> deleteUserFollowCoach authres

userProfileProxy :: Proxy UserProfileAPI
userProfileProxy = Proxy

userProfileServer :: Configuration -> AuthResult User -> Server UserProfileAPI
userProfileServer conf authres =
  hoistServer userProfileProxy (coachToHandler conf) (userProfileApi authres)
