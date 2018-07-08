{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Lib.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Time

-- | Body of the registration request.
data RequestRegistrationBody = RequestRegistrationBody
  { reqregbodyUsername :: Text -- ^ Username field.
  , reqregbodyEmail    :: Text -- ^ Email field.
  , reqregbodyPassword :: Text -- ^ Password field.
  } deriving (Generic)
instance FromJSON RequestRegistrationBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

-- | Request for the registration.
data RequestRegistration = RequestRegistration
  { reqregUser :: RequestRegistrationBody
  } deriving (Generic)
instance FromJSON RequestRegistration where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestLoginBody = RequestLoginBody
  { reqloginbodyEmail    :: Text
  , reqloginbodyPassword :: Text
  } deriving (Generic)
instance FromJSON RequestLoginBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestLogin = RequestLogin
  { reqloginUser :: RequestLoginBody
  } deriving (Generic)
instance FromJSON RequestLogin where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestUpdateUserBody = RequestUpdateUserBody
  { requpdtuserbodyEmail    :: Maybe Text
  , requpdtuserbodyBio      :: Maybe Text
  , requpdtuserbodyImage    :: Maybe Text
  , requpdtuserbodyUsername :: Maybe Text
  , requpdtuserbodyPassword :: Maybe Text
  } deriving (Generic)
instance FromJSON RequestUpdateUserBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestUpdateUser = RequestUpdateUser
  { requpdtuserUser :: RequestUpdateUserBody
  } deriving (Generic)
instance FromJSON RequestUpdateUser where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestCreateArticleBody = RequestCreateArticleBody
  { reqcrtarticlTitle       :: Text
  , reqcrtarticlDescription :: Text
  , reqcrtarticlBody        :: Text
  , reqcrtarticlTagList     :: Maybe [Text]
  } deriving Generic
instance FromJSON RequestCreateArticleBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestCreateArticle = RequestCreateArticle
  { reqcrtarticArticle :: RequestCreateArticleBody
  } deriving (Generic)
instance FromJSON RequestCreateArticle where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestUpdateArticleBody = RequestUpdateArticleBody
  { requpdtarticbodyTitle       :: Maybe Text
  , requpdtarticbodyDescription :: Maybe Text
  , requpdtarticbodyBody        :: Maybe Text
  } deriving Generic
instance FromJSON RequestUpdateArticleBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestUpdateArticle = RequestUpdateArticle
  { requpdtarticArticle :: RequestUpdateArticleBody
  } deriving (Generic)
instance FromJSON RequestUpdateArticle where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestCommentBody = RequestCommentBody
  { reqcmtbodyBody :: Text
  } deriving (Generic)
instance FromJSON RequestCommentBody where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data RequestComment = RequestComment
  { reqcmtComment :: RequestCommentBody
  } deriving (Generic)
instance FromJSON RequestComment where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

data ResponseUserBody = ResponseUserBody
  { respuserbodyEmail    :: Text
  , respuserbodyToken    :: Maybe Text
  , respuserbodyUsername :: Text
  , respuserbodyBio      :: Maybe Text
  , respuserbodyImage    :: Maybe Text
  } deriving (Generic)
instance ToJSON ResponseUserBody where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseUser = ResponseUser
  { responseuserUser :: ResponseUserBody
  } deriving (Generic)
instance ToJSON ResponseUser where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseProfileBody = ResponseProfileBody
  { respprofbodyUsername  :: Text
  , respprofbodyBio       :: Maybe Text
  , respprofbodyImage     :: Maybe Text
  , respprofbodyFollowing :: Bool
  } deriving (Generic)
instance ToJSON ResponseProfileBody where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseProfile = ResponseProfile
  { respprofProfile :: ResponseProfileBody
  } deriving (Generic)
instance ToJSON ResponseProfile where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseArticleBody = ResponseArticleBody
  { resparticbodySlug           :: Text
  , resparticbodyTitle          :: Text
  , resparticbodyDescription    :: Text
  , resparticbodyBody           :: Text
  , resparticbodyTagList        :: Maybe [Text]
  , resparticbodyCreatedAt      :: UTCTime
  , resparticbodyUpdatedAt      :: Maybe UTCTime
  , resparticbodyFavorited      :: Bool
  , resparticbodyFavoritesCount :: Int64
  , resparticbodyAuthor         :: ResponseProfileBody
  } deriving (Generic)
instance ToJSON ResponseArticleBody where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseArticle = ResponseArticle
  { resparticArticle :: ResponseArticleBody
  } deriving (Generic)
instance ToJSON ResponseArticle where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseMultiArticle = ResponseMultiArticle
  { respmultiarticArticles      :: [ResponseArticleBody]
  , respmultiarticArticlesCount :: Int
  } deriving (Generic)
instance ToJSON ResponseMultiArticle where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseCommentBody = ResponseCommentBody
  { respcomtbodyId         :: Int64
  , respcomtbodyCreatedAt  :: UTCTime
  , respcomtbodyUpdatedAtt :: Maybe UTCTime
  , respcomtbodyBody       :: Text
  , respcomtbodyAuthor     :: ResponseProfileBody
  } deriving (Generic)
instance ToJSON ResponseCommentBody where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseComment = ResponseComment
  { respcomtComment :: ResponseCommentBody
  } deriving (Generic)
instance ToJSON ResponseComment where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseMultiComment = ResponseMultiComment
  { respmulticomtComments :: [ResponseCommentBody]
  } deriving (Generic)
instance ToJSON ResponseMultiComment where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseTags = ResponseTags
  { resptagsTags :: [Text]
  } deriving (Generic)
instance ToJSON ResponseTags where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseErrorBody = ResponseErrorBody
  { resperrbodyBody :: Text
  } deriving (Generic)
instance ToJSON ResponseErrorBody where
  toJSON = genericToJSON (aesonPrefix camelCase)

data ResponseError = ResponseError
  { resperrErrors :: ResponseErrorBody
  } deriving (Generic)
instance ToJSON ResponseError where
  toJSON = genericToJSON (aesonPrefix camelCase)
