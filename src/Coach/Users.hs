{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Coach.Users where

import           Lib.Prelude

import           Crypto.BCrypt
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  as BL (toStrict)
import           Data.Text             (unpack)
import           Database.Esqueleto    hiding (isNothing)
import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Que.Users

postRegistrationCoach
  :: MonadIO m => RequestRegistration -> CoachT m ResponseUser
postRegistrationCoach (RequestRegistration reqreg) = do
  usernameEmailDontConflict (reqregbodyUsername reqreg) (reqregbodyEmail reqreg)
  hashedpassword  <- liftIO $ generatePassword (reqregbodyPassword reqreg)
  (Entity _ user) <- runDb $ insertUserEntity $ User
    (reqregbodyEmail reqreg)
    (reqregbodyUsername reqreg)
    hashedpassword
    Nothing
    Nothing
  token <- generateToken user
  return $ ResponseUser $ ResponseUserBody (userEmail user)
                                           (Just token)
                                           (userUsername user)
                                           (userBio user)
                                           (userImage user)
 where
  usernameEmailDontConflict username email = do
    existings <- runDb (selectUserByUsernameEmail username email)
    case existings of
      [] -> return ()
      _ ->
        throwError err409 { errBody = encodeRespError "User already exists." }

generateToken :: MonadIO m => User -> CoachT m Text
generateToken user = do
  jws    <- asks configurationJWTSettings
  etoken <- liftIO $ makeJWT user jws Nothing
  token  <- eitherToCoach etoken (decodeUtf8 . BL.toStrict) err500
  return token

eitherToCoach
  :: (Show a1, MonadError ServantErr m)
  => Either a1 t
  -> (t -> a2)
  -> ServantErr
  -> m a2
eitherToCoach (Left x) _ onFail = throwError $ onFail { errBody = show x }
eitherToCoach (Right v) onSuccess _ = return $ onSuccess v

postLoginCoach :: MonadIO m => RequestLogin -> CoachT m ResponseUser
postLoginCoach (RequestLogin (RequestLoginBody email password)) = do
  (Entity _ user) <- notFoundIfNothing =<< runDb (getBy (UniqueEmail email))
  unless
      ( validatePassword ((pack . unpack) (userPassword user))
                         ((pack . unpack) (password))
      )
    $ throwError err401 { errBody = encodeRespError "No such thing." }
  token <- generateToken user
  return $ ResponseUser $ ResponseUserBody (userEmail user)
                                           (Just token)
                                           (userUsername user)
                                           (userBio user)
                                           (userImage user)
 where
  notFoundIfNothing Nothing =
    throwError err401 { errBody = encodeRespError "No such thing." }
  notFoundIfNothing (Just x) = return x

getUserInformationCoach :: MonadIO m => AuthResult User -> CoachT m ResponseUser
getUserInformationCoach (Authenticated user) = do
  token <- generateToken user
  return $ ResponseUser $ ResponseUserBody (userEmail user)
                                           (Just token)
                                           (userUsername user)
                                           (userBio user)
                                           (userImage user)
getUserInformationCoach _ =
  throwError err401 { errBody = encodeRespError "Not qualified to access" }

putUserInformationCoach
  :: MonadIO m => AuthResult User -> RequestUpdateUser -> CoachT m ResponseUser
putUserInformationCoach (Authenticated _) (RequestUpdateUser (RequestUpdateUserBody Nothing Nothing Nothing Nothing Nothing))
  = throwError err422
    { errBody = encodeRespError "What are you going to update?"
    }
putUserInformationCoach (Authenticated user) (RequestUpdateUser requpdate) = do
  let newUsernameEmail old (Just x) = if old == x then Nothing else Just x
      newUsernameEmail _   Nothing  = Nothing
      perhapsnewusername =
        newUsernameEmail (userUsername user) (requpdtuserbodyUsername requpdate)
      perhapsnewemail =
        newUsernameEmail (userEmail user) (requpdtuserbodyEmail requpdate)
  existings <- runDb
    $ selectUserByMaybeUsernameEmail perhapsnewusername perhapsnewemail
  unless (null existings)
    $ throwError err409 { errBody = encodeRespError "Already used." }
  runDb $ updateUser (userUsername user)
                     (requpdtuserbodyEmail requpdate)
                     (requpdtuserbodyUsername requpdate)
                     (requpdtuserbodyPassword requpdate)
                     (requpdtuserbodyImage requpdate)
                     (requpdtuserbodyBio requpdate)
  (Just (Entity _ u)) <- runDb $ getBy $ UniqueEmail $ fromMaybe
    (userEmail user)
    perhapsnewemail
  token <- generateToken u
  return $ ResponseUser $ ResponseUserBody (userEmail u)
                                           (Just token)
                                           (userUsername u)
                                           (userBio u)
                                           (userImage u)
putUserInformationCoach _ _ =
  throwError err401 { errBody = encodeRespError "Not allowed to access." }
