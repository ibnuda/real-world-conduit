{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Coach.Users where

import           Protolude

import           Crypto.BCrypt
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  as BL (toStrict)
import           Data.Text             (unpack)
import           Database.Esqueleto
import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Que.Users

postRegistrationCoach :: MonadIO m => RequestRegistration -> CoachT m ResponseUser
postRegistrationCoach (RequestRegistration reqreg) = do
  (usernameEmailDontConflict
     (reqregbodyUsername reqreg)
     (reqregbodyEmail reqreg))
  hashedpassword <- liftIO $ generatePassword (reqregbodyPassword reqreg)
  (Entity _ user) <-
    runDb $
    insertUserEntity $
    User
      (reqregbodyEmail reqreg)
      (reqregbodyUsername reqreg)
      hashedpassword
      Nothing
      Nothing
  token <- generateToken user
  return $
    ResponseUser $
    ResponseUserBody
      (userEmail user)
      (Just token)
      (userUsername user)
      (userBio user)
      (userImage user)
  where
    usernameEmailDontConflict username email = do
      existings <- runDb (selectUserByUsernameEmail username email)
      case existings of
        [] -> return ()
        _ -> throwError err422
    generatePassword password = do
      mpass <-
        hashPasswordUsingPolicy
          slowerBcryptHashingPolicy
          (pack $ unpack password)
      case mpass of
        Nothing -> generatePassword password
        Just pa -> return (decodeUtf8 pa)
    generateToken user = do
      jws <- asks configurationJWTSettings
      etoken <- liftIO $ makeJWT user jws Nothing
      token <- eitherToCoach etoken (decodeUtf8 . BL.toStrict) err500
      return token
    eitherToCoach (Left x) _ onFail = throwError $ onFail {errBody = show x}
    eitherToCoach (Right v) onSuccess _ = return $ onSuccess v

postLoginCoach :: MonadIO m => RequestLogin -> CoachT m ResponseUser
postLoginCoach (RequestLogin (RequestLoginBody email password)) = panic ""

getUserInformationCoach :: MonadIO m => AuthResult User -> CoachT m ResponseUser
getUserInformationCoach (Authenticated user) = panic ""
getUserInformationCoach _                    = throwError err401

putUserInformationCoach :: MonadIO m => AuthResult User -> RequestUpdateUser -> CoachT m ResponseUser
putUserInformationCoach (Authenticated user) (RequestUpdateUser reqbody) = panic ""
putUserInformationCoach _ _ = throwError err401

