{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module API.User where

import           Lib.Prelude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Coach.Users

type UserInformationAPI =
  "user"
    :> Get '[ JSON] ResponseUser
  :<|> "user"
    :> ReqBody '[ JSON] RequestUpdateUser
    :> Put '[ JSON] ResponseUser

userInformationApi ::
     MonadIO m => AuthResult User -> ServerT UserInformationAPI (CoachT m)
userInformationApi authres =
  getUserInformationCoach authres :<|> putUserInformationCoach authres

userInformationProxy :: Proxy UserInformationAPI
userInformationProxy = Proxy

userInformationServer ::
     Configuration -> AuthResult User -> Server UserInformationAPI
userInformationServer conf authres =
  hoistServer
    userInformationProxy
    (coachToHandler conf)
    (userInformationApi authres)

type UserAdministrationAPI =
  "users"
    :> ReqBody '[ JSON] RequestRegistration
    :> Post '[ JSON] ResponseUser
  :<|> "users"
    :> "login"
    :> ReqBody '[JSON] RequestLogin
    :> Post '[ JSON] ResponseUser

userAdministrationApi :: MonadIO m => ServerT UserAdministrationAPI (CoachT m)
userAdministrationApi = postRegistrationCoach :<|> postLoginCoach

userAdministrationProxy :: Proxy UserAdministrationAPI
userAdministrationProxy = Proxy

userAdministrationServer :: Configuration -> Server UserAdministrationAPI
userAdministrationServer conf =
  hoistServer
    userAdministrationProxy
    (coachToHandler conf)
    userAdministrationApi
