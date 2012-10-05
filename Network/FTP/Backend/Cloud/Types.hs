{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           , MultiParamTypeClasses
           #-}

module Network.FTP.Backend.Cloud.Types where

import qualified Prelude
import BasicPrelude
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Conduit
import Network.HTTP.Conduit
import Network.Aliyun (YunConf)

data CloudService m = CloudService
  { listBuckets :: m [ByteString]
  , listObjects :: ByteString -> ByteString -> m [ByteString]
  , putBucket   :: ByteString -> m ()
  , getObject   :: ByteString -> ByteString -> Source m ByteString
  , putObject   :: ByteString -> ByteString -> Sink ByteString m ()
  , renameObject :: ByteString -> ByteString -> ByteString -> m ()
  , removeObject :: ByteString -> ByteString -> m ()
  }

emptyCloudService :: CloudService CloudBackend
emptyCloudService =
  CloudService { listBuckets = fail "not initialized."
               , listObjects = \_ _ -> fail "not initialized."
               , putBucket   = const $ fail "not initialized."
               , getObject   = \_ _ -> lift (fail "not initialized.")
               , putObject   = \_ _ -> lift (fail "not initialized.")
               , renameObject = \_ _ _ -> fail "not initialized."
               , removeObject = \_ _ -> fail "not initialized."
               }

data CloudConf = CloudConf
  { aliyunConf   :: YunConf
  }

type CloudEnv = (CloudConf, Manager)

-- store user related information
data CloudState = CloudState
  { stService :: CloudService CloudBackend
  }

newtype CloudBackend a = CloudBackend { unCloudBackend :: RWST CloudEnv () CloudState (ResourceT IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadUnsafeIO, MonadThrow, MonadResource
             )

instance MonadBase IO CloudBackend where
    liftBase = CloudBackend . liftBase

instance MonadBaseControl IO CloudBackend where
    newtype StM CloudBackend a = CloudBackendStM { unCloudBackendStM :: StM (RWST CloudEnv () CloudState (ResourceT IO)) a }
    liftBaseWith f = CloudBackend . liftBaseWith $ \runInBase -> f $ liftM CloudBackendStM . runInBase . unCloudBackend
    restoreM = CloudBackend . restoreM . unCloudBackendStM

runCloudBackend :: CloudConf -> CloudBackend a -> IO a
runCloudBackend conf m =
    fmap fst $
        withManager $ \man ->
            evalRWST (unCloudBackend m) (conf, man) (CloudState emptyCloudService)

withService :: (CloudService CloudBackend -> CloudBackend a) -> CloudBackend a
withService f = CloudBackend (gets stService) >>= f

