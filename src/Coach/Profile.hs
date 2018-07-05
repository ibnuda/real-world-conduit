{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Coach.Profile where

import           Lib.Prelude

import           Database.Esqueleto  hiding (isNothing)
import           Servant
import           Servant.Auth.Server
import           Servant.Server

import           Conf
import           Model
import           Types

import           Que.Users

getUserProfileCoach ::
     MonadIO m
  => AuthResult User
  -> Text
  -> CoachT m ResponseProfile
getUserProfileCoach authres profilename = do
  muser <- runDb $ getBy $ UniqueUsername profilename
  when (isNothing muser) $ throwError err404 {errBody = "No such profile."}
  followings <-
    runDb $
    selectFollowsByUsernameAndProfilename
      (userUsername <$> authresToMaybe authres)
      profilename
  case followings of
    [] -> throwError err410
    ((Entity _ user, Value follow):_) -> do
      return $
        ResponseProfile $
        ResponseProfileBody
          (userUsername user)
          (userBio user)
          (userImage user)
          follow
  where
    authresToMaybe (Authenticated x) = Just x
    authresToMaybe _                 = Nothing

postUserFollowCoach ::
     MonadIO m => AuthResult User -> Text -> CoachT m NoContent
postUserFollowCoach (Authenticated user) profilename = do
  muser <- runDb $ getBy $ UniqueUsername profilename
  follows <- runDb $ selectFollows (userUsername user) profilename
  unless (null follows || isNothing muser) $
    throwError err409 {errBody = "Already followed that profile"}
  runDb $ insertFollows (userUsername user) profilename
  return NoContent
postUserFollowCoach _ _                              = throwError err401

deleteUserFollowCoach :: MonadIO m => AuthResult User -> Text -> CoachT m NoContent
deleteUserFollowCoach (Authenticated user) profilename = do
  muser <- runDb $ getBy $ UniqueUsername profilename
  follows <- runDb $ selectFollows (userUsername user) profilename
  when (null follows || isNothing muser) $
    throwError err404 {errBody = "You are not following that profile"}
  runDb $ deleteFollows (userUsername user) profilename
  return NoContent
deleteUserFollowCoach _ _                              = throwError err401
