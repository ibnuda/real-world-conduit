{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Que.Comments where

import           Lib.Prelude        hiding (from, get, on, (<&>))

import           Database.Esqueleto

import           Model

selectComments ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Maybe Text
  -> Text
  -> ReaderT backend m [(Entity Comment, Entity User, Value Bool)]
selectComments musername slug = do
  select $
    from $ \(article `InnerJoin` comment `InnerJoin` commentator) -> do
      on $ comment ^. CommentUserId ==. commentator ^. UserId
      on $ article ^. ArticleId ==. comment ^. CommentArticleId
      let following Nothing = val False
          following (Just x) =
            case_
              [ when_
                  (exists $
                   from $ \(user, follow) -> do
                     where_ $ user ^. UserId ==. follow ^. FollowFollowerId
                     where_ $ commentator ^. UserId ==. follow ^. FollowAuthorId
                     where_ $ user ^. UserUsername ==. val x)
                  then_ $
                val True
              ]
              (else_ $ val False)
      where_ $ article ^. ArticleSlug ==. val slug
      orderBy [asc (comment ^. CommentId)]
      return (comment, commentator, following musername)
