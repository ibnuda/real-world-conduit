{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Coach.Profile where

import           Lib.Prelude

import           Database.Esqueleto  hiding (isNothing)
import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types
import           Util

import           Que.Users

getUserProfileCoach
  :: MonadIO m => AuthResult User -> Text -> CoachT m ResponseProfile
getUserProfileCoach authres profilename = do
  muser <- runDb $ getBy $ UniqueUsername profilename
  when (isNothing muser)
    $ throwError err404 { errBody = encodeRespError "No such profile." }
  followings <- runDb $ selectFollowsByUsernameAndProfilename
    (userUsername <$> authresToMaybe authres)
    profilename
  case followings of
    [] -> throwError err410 { errBody = encodeRespError "Is gone!" }
    ((Entity _ user, Value follow):_) -> do
      return $ ResponseProfile $ ResponseProfileBody (userUsername user)
                                                     (userBio user)
                                                     (userImage user)
                                                     follow

postUserFollowCoach
  :: MonadIO m => AuthResult User -> Text -> CoachT m NoContent
postUserFollowCoach (Authenticated user) profilename = do
  muser <- runDb $ getBy $ UniqueUsername profilename
  when (isNothing muser) $ throwError err404
    { errBody = encodeRespError "There are no such user."
    }
  follows <- runDb $ selectFollows (userUsername user) profilename
  unless (null follows) $ throwError err409
    { errBody = encodeRespError "Already followed that profile."
    }
  runDb $ insertFollows (userUsername user) profilename
  return NoContent
postUserFollowCoach _ _ =
  throwError err401 { errBody = encodeRespError "Not allowed to access." }

deleteUserFollowCoach
  :: MonadIO m => AuthResult User -> Text -> CoachT m NoContent
deleteUserFollowCoach (Authenticated user) profilename = do
  muser <- runDb $ getBy $ UniqueUsername profilename
  when (isNothing muser) $ throwError err404
    { errBody = encodeRespError "There are no such user."
    }
  follows <- runDb $ selectFollows (userUsername user) profilename
  when (null follows) $ throwError err404
    { errBody = encodeRespError "You are not following that profile"
    }
  runDb $ deleteFollows (userUsername user) profilename
  return NoContent
deleteUserFollowCoach _ _ =
  throwError err401 { errBody = encodeRespError "Not allowed to access." }
