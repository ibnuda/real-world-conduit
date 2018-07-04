-- | Running your app inside GHCi.
--
-- > stack ghci
--
-- To start your app, run:
--
-- > :l DevelMain
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci

module DevelMain where

import           Protolude

import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Foreign.Store            (Store (..), lookupStore, readStore,
                                           storeAction, withStore)
import           GHC.Word                 (Word32)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setPort)

import           RealWorld

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore =
      modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start
    start :: MVar () -> IO ThreadId
    start done = do
      (port, app) <- startDevel
      forkFinally
        (runSettings (setPort port defaultSettings) app)
        (\_ -> putMVar done () >> stop)

-- | kill the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> putText "no app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putText "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f =
  withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
