{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Que.Comments where

import           Lib.Prelude        hiding (from, get, on, (<&>))

import           Data.Time
import           Database.Esqueleto

import           Model

selectComments
  :: ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Maybe Text
  -> Text
  -> ReaderT backend m [(Entity Comment, Entity User, Value Bool)]
selectComments musername slug = do
  select $ from $ \(article `InnerJoin` comment `InnerJoin` commentator) -> do
    on $ comment ^. CommentUserId ==. commentator ^. UserId
    on $ article ^. ArticleId ==. comment ^. CommentArticleId
    let following Nothing  = val False
        following (Just x) = case_
          [ when_
                ( exists $ from $ \(user, follow) -> do
                  where_ $ user ^. UserId ==. follow ^. FollowFollowerId
                  where_ $ commentator ^. UserId ==. follow ^. FollowAuthorId
                  where_ $ user ^. UserUsername ==. val x
                )
                then_
              $ val True
          ]
          (else_ $ val False)
    where_ $ article ^. ArticleSlug ==. val slug
    orderBy [asc (comment ^. CommentId)]
    return (comment, commentator, following musername)

insertComment
  :: ( BaseBackend backend ~ SqlBackend
     , MonadIO m
     , BackendCompatible SqlBackend backend
     , PersistQueryRead backend
     , PersistUniqueRead backend
     , PersistStoreWrite backend
     )
  => Text
  -> Text
  -> Text
  -> ReaderT
       backend
       m
       (Maybe (Entity Comment, Entity User, Value Bool))
insertComment username slug body = do
  now       <- liftIO getCurrentTime
  something <- select $ from $ \(user, article) -> do
    let narcissticprick Nothing      = val False
        narcissticprick (Just uname) = case_
          [ when_
                ( exists $ from $ \(u, f) -> do
                  where_ $ u ^. UserId ==. f ^. FollowFollowerId
                  where_ $ u ^. UserId ==. f ^. FollowAuthorId
                  where_ $ u ^. UserUsername ==. val uname
                )
                then_
              $ val True
          ]
          (else_ $ val False)
    where_ $ user ^. UserUsername ==. val username
    where_ $ article ^. ArticleSlug ==. val slug
    return (article ^. ArticleId, user, narcissticprick (Just username))
  case something of
    [] -> return Nothing
    (articleid, entuser, valfollow):_ -> do
      comment <- insertEntity
        $ Comment body now Nothing (unValue articleid) (entityKey entuser)
      return $ Just (comment, entuser, valfollow)
