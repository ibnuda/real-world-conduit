module Util where

import           Lib.Prelude

import           Crypto.BCrypt
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Lazy  as BL
import           Data.Text             (pack, toLower, unpack)
import           Servant.Auth.Server
import           Text.Regex

import           Types

generatePassword :: Text -> IO Text
generatePassword password = do
  mpass <-
    hashPasswordUsingPolicy
      slowerBcryptHashingPolicy
      (BC.pack $ unpack password)
  case mpass of
    Nothing -> generatePassword password
    Just pa -> return (decodeUtf8 pa)

authresToMaybe :: AuthResult a -> Maybe a
authresToMaybe (Authenticated x) = Just x
authresToMaybe _                 = Nothing

titleDescToSlug :: Text -> Text -> Text -> Text
titleDescToSlug title desc appendage =
  smaller title <> "-" <> smaller desc <> "-" <> smaller appendage
  where
    smaller sentence =
      toLower (pack (subRegex (mkRegex "[^a-zA-Z0-9_.]") (unpack sentence) "-"))

encodeRespError :: Text -> BL.ByteString
encodeRespError = encode . ResponseError . ResponseErrorBody
