module Lib
  ( someFunc
  ) where

import           Lib.Prelude

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: Text)
