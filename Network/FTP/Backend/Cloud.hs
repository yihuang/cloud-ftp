{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings
           , TypeFamilies
           , MultiParamTypeClasses
           #-}
module Network.FTP.Backend.Cloud
  ( CloudConf(..)
  , CloudBackend(..)
  , runCloudBackend
  ) where

{-|
 - Cloud storage based backend.
 -}

import qualified Prelude
import BasicPrelude
import System.Directory
import Filesystem.Path.CurrentOS

import Control.Monad.Trans.RWS
import Control.Monad.Trans.Control
import Control.Monad.Base

import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Process as C
import Network.HTTP.Conduit

import Network.FTP.Backend (FTPBackend(..))
import Network.Aliyun

data CloudConf = CloudConf
  { httpManager  :: Manager
  }

-- store user related information
data CloudState = CloudState

newtype CloudBackend a = CloudBackend { unCloudBackend :: RWST CloudConf () CloudState (ResourceT IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadUnsafeIO, MonadThrow, MonadResource
             )

instance MonadBase IO CloudBackend where
    liftBase = CloudBackend . liftBase

instance MonadBaseControl IO CloudBackend where
    newtype StM CloudBackend a = CloudBackendStM { unCloudBackendStM :: StM (RWST CloudConf () CloudState (ResourceT IO)) a }
    liftBaseWith f = CloudBackend . liftBaseWith $ \runInBase -> f $ liftM CloudBackendStM . runInBase . unCloudBackend
    restoreM = CloudBackend . restoreM . unCloudBackendStM

runCloudBackend :: CloudBackend a -> IO a
runCloudBackend m = fmap fst $ withManager $ \man -> evalRWST (unCloudBackend m) (CloudConf man) CloudState

instance FTPBackend CloudBackend where
    type UserId CloudBackend = ByteString

    ftplog = liftIO . S.putStrLn

    authenticate user pass =
        if user==pass
          then return (Just user)
          else return Nothing

    list dir =
        return ()

    nlst dir =
        return ()

    mkd dir =
        return ()

    dele name =
        return ()

    rename fname tname = do
        return ()

    rmd dir = do
        return ()

    download name =
        return ()

    upload   name =
        return ()
