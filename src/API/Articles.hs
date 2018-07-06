{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module API.Articles where

import           Lib.Prelude

import           Servant
import           Servant.Auth.Server

import           Conf
import           Model
import           Types

import           Coach.Articles

type ArticlesAPI =
  "articles"
    :> QueryParam "tag" Text
    :> QueryParam "author" Text
    :> QueryParam "favorited" Text
    :> QueryParam "limit" Int64
    :> QueryParam "offset" Int64
    :> Get '[ JSON] ResponseMultiArticle
  :<|> "articles"
    :> "feed"
    :> QueryParam "limit" Int64
    :> QueryParam "offset" Int64
    :> Get '[ JSON] ResponseMultiArticle
  :<|> "articles"
    :> Capture "slug" Text
    :> Get '[ JSON] ResponseArticle
  :<|> "articles"
    :> ReqBody '[ JSON] RequestCreateArticle
    :> Post '[ JSON] ResponseArticle
  :<|> "articles"
    :> Capture "slug" Text
    :> DeleteNoContent '[ JSON] NoContent
  :<|> "articles"
    :> Capture "slug" Text
    :> ReqBody '[ JSON] RequestUpdateArticle
    :> Put '[ JSON] ResponseArticle
  :<|> "articles"
    :> Capture "slug" Text
    :> "comments"
    :> Get '[JSON] ResponseMultiComment
  :<|> "articles"
    :> Capture "slug" Text
    :> "comments"
    :> ReqBody '[ JSON] RequestComment
    :> Post '[JSON] ResponseComment
  :<|> "articles"
    :> Capture "slug" Text
    :> "comments"
    :> Capture "id" Int64
    :> DeleteNoContent '[ JSON] NoContent
  :<|> "articles"
    :> Capture "slug" Text
    :> "favorite"
    :> PostNoContent '[ JSON] NoContent
  :<|> "articles"
    :> Capture "slug" Text
    :> "favorite"
    :> DeleteNoContent '[ JSON] NoContent

articlesProxy :: Proxy ArticlesAPI
articlesProxy = Proxy

articlesApi :: MonadIO m => AuthResult User -> ServerT ArticlesAPI (CoachT m)
articlesApi authres =
  getArticlesCoach authres
  :<|> getArticlesFeed authres
  :<|> getArticleSlugCoach authres
  :<|> postArticleCreateCoach authres
  :<|> deleteArticleSlugCoach authres
  :<|> putArticleSlugCoach authres
  :<|> panic ""

articlesServer :: Configuration -> AuthResult User -> Server ArticlesAPI
articlesServer conf authres =
  hoistServer articlesProxy (coachToHandler conf) (articlesApi authres)
