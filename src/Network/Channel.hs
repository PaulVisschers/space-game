{-# LANGUAGE RankNTypes #-}
module Network.Channel where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Monad (forever, liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Binary (Binary, decode, encode)
import Data.ByteString.Lazy as BS (ByteString, append, hGet, hPut, length)
import Data.Int (Int64)
import Network as N (HostName, PortID(PortNumber), PortNumber, Socket, accept, connectTo, listenOn, withSocketsDo)
import System.IO (Handle, hFlush)

withSockets :: IO a -> IO a
withSockets = withSocketsDo

data Channel i o = Channel Handle (MVar ByteString)

connect :: MonadIO m => HostName -> PortNumber -> m (Channel i o)
connect hn pn = liftIO $ do
  h <- connectTo hn (PortNumber pn)
  mv <- newEmptyMVar
  forkIO (forever (forkedReceive h mv))
  return (Channel h mv)

listen :: MonadIO m => PortNumber -> m Socket
listen pn = liftIO (listenOn (PortNumber pn))

accept :: MonadIO m => Socket -> m (Channel i o)
accept s = liftIO $ do
  (h, _, _) <- N.accept s
  mv <- newEmptyMVar
  forkIO (forever (forkedReceive h mv))
  return (Channel h mv)

forkedReceive :: MonadIO m => Handle -> MVar ByteString -> m ()
forkedReceive h mv = liftIO $ do
  bs <- hGet h 8
  let len = fromIntegral (decode bs :: Int64)
  bs <- hGet h len
  putMVar mv bs

send :: (Binary o, MonadIO m) => Channel i o -> o -> m ()
send (Channel h mv) x = liftIO $ do
  let bs = encode x
  let len = encode (BS.length bs)
  hPut h (append len bs)
  hFlush h

ready :: (Binary i, MonadIO m) => Channel i o -> m Bool
ready (Channel h mv) = liftIO $ do
  b <- isEmptyMVar mv
  return (not b)

receive :: (Binary i, MonadIO m) => Channel i o -> m i
receive (Channel h mv) = liftIO $ do
  bs <- takeMVar mv
  return (decode bs)

tryReceive :: (Binary i, MonadIO m) => Channel i o -> m (Maybe i)
tryReceive (Channel h mv) = liftIO $ do
  mbs <- tryTakeMVar mv
  return (decode `liftM` mbs)