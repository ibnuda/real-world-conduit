{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Que.Articles where

import           Lib.Prelude                          hiding (from, get, on,
                                                       (<&>))

import           Data.Time
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Language
import           Database.Esqueleto.PostgreSQL

import           Model

selectArticles ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Maybe Text
  -> Bool
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Int64
  -> Int64
  -> ReaderT backend m [( Entity Article
                        , Entity User
                        , Value (Maybe [Text])
                        , Value Int64
                        , Value Bool
                        , Value Bool)]
selectArticles musername isfeed mslug mtag mauthor mfavoritedby lim off = do
  select $
    from $ \(article
             `InnerJoin` author
             `LeftOuterJoin` tagged
             `LeftOuterJoin` tags
             `LeftOuterJoin` favorited
             `LeftOuterJoin` favoriter) -> do
      on (favorited ^. FavoritedUserId ==. favoriter ^. UserId)
      on (favorited ^. FavoritedArticleId ==. article ^. ArticleId)
      on (tags ^. TagId ==. tagged ^. TaggedTagId)
      on (article ^. ArticleId ==. tagged ^. TaggedArticleId)
      on (author ^. UserId ==. article ^. ArticleAuthorId)
      let tagnames =
            sub_select $
            from $ \(tagged', tags') -> do
              where_ (tagged' ^. TaggedArticleId ==. article ^. ArticleId)
              where_ (tagged' ^. TaggedTagId ==. tags' ^. TagId)
              return $ arrayAgg $ tags' ^. TagName
          favoritecounts =
            sub_select $
            from $ \(favorited', user) -> do
              where_ (favorited' ^. FavoritedUserId ==. user ^. UserId)
              where_ (favorited' ^. FavoritedArticleId ==. article ^. ArticleId)
              return $ count $ user ^. UserId
          favoritingArticle Nothing = val False
          favoritingArticle (Just username) =
            case_
              [ when_
                  (exists $
                   from $ \(favorited', user) -> do
                     where_
                       (favorited' ^. FavoritedArticleId ==. article ^.
                        ArticleId)
                     where_ (favorited' ^. FavoritedUserId ==. user ^. UserId)
                     where_ (user ^. UserUsername ==. val username))
                  then_ $
                val True
              ]
              (else_ $ val False)
          followingAuthor Nothing = val False
          followingAuthor (Just username) =
            case_
              [ when_
                  (exists $
                   from $ \(follow, user) -> do
                     where_ (follow ^. FollowAuthorId ==. author ^. UserId)
                     where_ (follow ^. FollowFollowerId ==. user ^. UserId)
                     where_ (user ^. UserUsername ==. val username))
                  then_ $
                val True
              ]
              (else_ $ val False)
      whereMaybe mslug article ArticleSlug
      whereMaybe mtag tags TagName
      whereMaybe mauthor author UserUsername
      whereMaybe mfavoritedby favoriter UserUsername
      whereSublist isfeed musername author UserId subscribedAuthorQuery
      limit lim
      offset off
      return
        ( article
        , author
        , tagnames
        , favoritecounts
        , favoritingArticle musername
        , followingAuthor musername)

whereMaybe ::
     (PersistField typ, Esqueleto query expr backend, PersistEntity val)
  => Maybe typ
  -> expr (Entity val)
  -> EntityField val typ
  -> query ()
whereMaybe Nothing _ _              = return ()
whereMaybe (Just x) entity accessor = where_ $ entity ^. accessor ==. val x

whereSublist ::
     (PersistField typ, Esqueleto query expr backend, PersistEntity val)
  => Bool
  -> Maybe t
  -> expr (Entity val)
  -> EntityField val typ
  -> (t -> query (expr (Value typ)))
  -> query ()
whereSublist True (Just username) ent acc q = where_ $ ent ^. acc `in_` (subList_select $ q username)
whereSublist True Nothing _ _ _ = return ()
whereSublist False _ _ _ _ = return ()

subscribedAuthorQuery ::
     ( FromPreprocess query expr backend (expr (Entity Follow))
     , FromPreprocess query expr backend (expr (Entity User))
     )
  => Text
  -> query (expr (Value (Key User)))
subscribedAuthorQuery username = do
  from $ \(user `InnerJoin` follow `InnerJoin` author) -> do
    on $ follow ^. FollowAuthorId ==. author ^. UserId
    on $ follow ^. FollowFollowerId ==. user ^. UserId
    where_ $ user ^. UserUsername ==. val username
    return $ author ^. UserId


insertArticle ::
     ( PersistUniqueWrite backend
     , PersistQueryWrite backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> ReaderT backend m ()
insertArticle username slug title descrip body = do
  now <- liftIO getCurrentTime
  insertSelect $
    from $ \user -> do
      where_ (user ^. UserUsername ==. val username)
      return $
        Article
        <# val slug
        <&> (user ^. UserId)
        <&> val title
        <&> val descrip
        <&> val body
        <&> val now
        <&> nothing

upsertMaybeTags ::
     ( BaseBackend backend ~ SqlBackend
     , PersistQueryWrite backend
     , BackendCompatible SqlBackend backend
     , PersistUniqueWrite backend
     , MonadIO m
     )
  => Maybe [Text]
  -> Text
  -> ReaderT backend m ()
upsertMaybeTags Nothing _ = return ()
upsertMaybeTags (Just tags) slug = do
  putMany $ map Tag tags
  insertSelect $
    from $ \(article, tag) -> do
      where_ $ article ^. ArticleSlug ==. val slug
      where_ $ tag ^. TagName `in_` valList tags
      return $
        Tagged
        <# (article ^. ArticleId)
        <&> (tag ^. TagId)
