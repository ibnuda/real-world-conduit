{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Coach.Articles where

import           Lib.Prelude

import qualified Data.Text           as T
import           Database.Esqueleto  hiding (isNothing)
import           Servant
import           Servant.Auth.Server
import           System.Random

import           Conf
import           Model
import           Types
import           Util

import           Que.Articles
import           Que.Comments

getArticlesCoach ::
     MonadIO m
  => AuthResult User
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int64
  -> Maybe Int64
  -> CoachT m ResponseMultiArticle
getArticlesCoach authres mtag mauthor mfavorited mlimit moffset = do
  articles <-
    runDb $
    selectArticles
      (userUsername <$> authresToMaybe authres)
      False -- is feed?
      Nothing -- slug name?
      mtag
      mauthor
      mfavorited
      (fromMaybe 20 mlimit)
      (fromMaybe 0 moffset)
  return $
    ResponseMultiArticle
      (map resultQueryToResponseArticle articles)
      (length articles)

getArticleSlugCoach ::
     MonadIO m
  => AuthResult User
  -> Text
  -> CoachT m ResponseArticle
getArticleSlugCoach authres slug = do
  articles <-
    runDb $
    selectArticles
      (userUsername <$> authresToMaybe authres)
      False
      (Just slug)
      Nothing
      Nothing
      Nothing
      1
      0
  case articles of
    []  -> throwError err404 {errBody = "No such article."}
    x:_ -> return $ ResponseArticle $ resultQueryToResponseArticle x

getArticlesFeed ::
     MonadIO m
  => AuthResult User
  -> Maybe Int64
  -> Maybe Int64
  -> CoachT m ResponseMultiArticle
getArticlesFeed (Authenticated User {..}) mlimit moffset = do
  articles <-
    runDb $
    selectArticles
      (Just userUsername)
      True
      Nothing
      Nothing
      Nothing
      Nothing
      (fromMaybe 20 mlimit)
      (fromMaybe 0 moffset)
  return $
    ResponseMultiArticle
      (map resultQueryToResponseArticle articles)
      (length articles)
getArticlesFeed _ _ _ = throwError err401 {errBody = "Only authenticated user."}

postArticleCreateCoach ::
     MonadIO m
  => AuthResult User
  -> RequestCreateArticle
  -> CoachT m ResponseArticle
postArticleCreateCoach (Authenticated User {..}) (RequestCreateArticle RequestCreateArticleBody {..}) = do
  randgen <- liftIO newStdGen
  let appendage = T.pack $ take 10 $ randomRs ('a', 'z') randgen
      slug = titleDescToSlug reqcrtarticlTitle reqcrtarticlDescription appendage
  articles <-
    runDb $ do
      insertArticle
        userUsername
        slug
        reqcrtarticlTitle
        reqcrtarticlDescription
        reqcrtarticlBody
      upsertMaybeTags reqcrtarticlTagList slug
      selectArticles
        (Just userUsername)
        False
        (Just slug)
        Nothing
        Nothing
        Nothing
        1
        0
  case articles of
    []  -> throwError err410 {errBody = "Should be created, but now it's gone."}
    x:_ -> return $ ResponseArticle $ resultQueryToResponseArticle x
postArticleCreateCoach _ _ = throwError err401

deleteArticleSlugCoach ::
     MonadIO m
  => AuthResult User
  -> Text
  -> CoachT m NoContent
deleteArticleSlugCoach (Authenticated User {..}) slug = do
  users <- runDb $ isArticleAuthor userUsername slug
  when (null users) $ throwError err401 {errBody = "Not the author or article doesn't exist."}
  runDb $ deleteArticle slug
  return NoContent
deleteArticleSlugCoach _ _ = throwError err401

putArticleSlugCoach ::
     MonadIO m
  => AuthResult User
  -> Text
  -> RequestUpdateArticle
  -> CoachT m ResponseArticle
putArticleSlugCoach (Authenticated User {..}) slug (RequestUpdateArticle req@RequestUpdateArticleBody {..}) = do
  users <- runDb $ isArticleAuthor userUsername slug
  when (null users) $
    throwError err401 {errBody = "Not the author or article doesn't exist."}
  when (reqUpdateIsEmpty req) $ throwError err422 {errBody = "u wot m8?"}
  articles <-
    runDb $ do
      updateArticle
        slug
        requpdtarticbodyTitle
        requpdtarticbodyDescription
        requpdtarticbodyBody
      selectArticles
        (Just userUsername)
        False
        (Just slug)
        Nothing
        Nothing
        Nothing
        1
        0
  case articles of
    []  -> throwError err410 {errBody = "Should be created, but now it's gone."}
    x:_ -> return $ ResponseArticle $ resultQueryToResponseArticle x
putArticleSlugCoach _ _ _ = throwError err401

reqUpdateIsEmpty :: RequestUpdateArticleBody -> Bool
reqUpdateIsEmpty RequestUpdateArticleBody {..} =
  isNothing requpdtarticbodyBody
  && isNothing requpdtarticbodyDescription
  && isNothing requpdtarticbodyTitle

getCommentsSlugCoach ::
     MonadIO m => AuthResult User -> Text -> CoachT m ResponseMultiComment
getCommentsSlugCoach authres slug = do
  marticle <- runDb $ getBy $ UniqueSlug slug
  when (isNothing marticle) $
    throwError err404 {errBody = "There's no such thing."}
  comments <-
    runDb $ selectComments (userUsername <$> authresToMaybe authres) slug
  return $ ResponseMultiComment $ map resultQueryToResponseComment comments

postCommentSlugCoach :: MonadIO m => AuthResult User -> Text -> RequestComment -> CoachT m ResponseComment
postCommentSlugCoach (Authenticated User {..}) slug reqcomment = do
  marticle <- runDb $ getBy $ UniqueSlug slug
  when (isNothing marticle) $
    throwError err404 {errBody = "There's no such thing."}
  panic ""
postCommentSlugCoach _ _ _ = throwError err401

resultQueryToResponseComment ::
     (Entity Comment, Entity User, Value Bool) -> ResponseCommentBody
resultQueryToResponseComment ((Entity cid Comment {..}), (Entity _ User {..}), (Value isfollowing)) =
  let commid = fromSqlKey cid
   in ResponseCommentBody
        commid
        commentCreatedAt
        commentUpdatedAt
        commentBody
        (ResponseProfileBody userUsername userBio userImage isfollowing)

resultQueryToResponseArticle ::
     ( Entity Article
     , Entity User
     , Value (Maybe [Text])
     , Value Int64
     , Value Bool
     , Value Bool)
  -> ResponseArticleBody
resultQueryToResponseArticle (entarticle, entauthor, vtags, vfavcounts, vfaving, vfoll) =
  let Article {..} = entityVal entarticle
      User {..} = entityVal entauthor
      tagnames = unValue vtags
      favcounts = unValue vfavcounts
      isfavoriting = unValue vfaving
      isfollowing = unValue vfoll
   in ResponseArticleBody
        articleSlug
        articleTitle
        articleDescription
        articleBody
        tagnames
        articleCreatedAt
        articleUpdatedAt
        isfavoriting
        favcounts $
      ResponseProfileBody userUsername userBio userImage isfollowing
