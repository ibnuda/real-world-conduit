module Util where

import           Lib.Prelude

import           Crypto.BCrypt
import           Data.ByteString.Char8 (pack)
import           Data.Text             (unpack)
import           Servant.Auth.Server

generatePassword :: Text -> IO Text
generatePassword password = do
  mpass <-
    hashPasswordUsingPolicy
      slowerBcryptHashingPolicy
      (pack $ unpack password)
  case mpass of
    Nothing -> generatePassword password
    Just pa -> return (decodeUtf8 pa)

authresToMaybe :: AuthResult a -> Maybe a
authresToMaybe (Authenticated x) = Just x
authresToMaybe _                 = Nothing
