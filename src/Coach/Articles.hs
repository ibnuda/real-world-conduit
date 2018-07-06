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
getArticlesFeed (Authenticated user) mlimit moffset = do
  articles <-
    runDb $
    selectArticles
      (Just $ userUsername user)
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

postArticleCreateCoach ::
     MonadIO m
  => AuthResult User
  -> RequestCreateArticle
  -> CoachT m ResponseArticle
postArticleCreateCoach (Authenticated user) (RequestCreateArticle RequestCreateArticleBody {..}) = do
  randgen <- liftIO newStdGen
  let appendage = T.pack $ take 10 $ randomRs ('a', 'z') randgen
      slug = titleDescToSlug reqcrtarticlTitle reqcrtarticlDescription appendage
  liftIO $ print reqcrtarticlTagList
  articles <-
    runDb $ do
      insertArticle
        (userUsername user)
        slug
        reqcrtarticlTitle
        reqcrtarticlDescription
        reqcrtarticlBody
      upsertMaybeTags reqcrtarticlTagList slug
      selectArticles
        (Just $ userUsername user)
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
postArticleCreateCoach _ _ = throwError err401
