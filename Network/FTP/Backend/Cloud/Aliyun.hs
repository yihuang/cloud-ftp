{-# LANGUAGE OverloadedStrings #-}
module Network.FTP.Backend.Cloud.Aliyun where

import qualified Prelude as P
import BasicPrelude
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Text.Format (format, left, Shown(Shown))
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Network.Aliyun as Ali
import Control.Monad.Trans.RWS (asks)
import Network.FTP.Backend.Cloud.Types (CloudBackend(..), CloudService(..))

run :: Ali.YunConf -> Ali.Yun a -> CloudBackend a
run conf yun = do
    man <- CloudBackend (asks snd)
    CloudBackend $ lift $ Ali.runYunWithManager man conf yun

printBucketContent :: Ali.BucketContent -> ByteString
printBucketContent a = case a of
    Ali.ContentDirectory dire time ->
        T.encodeUtf8 $ L.toStrict $
            format "drwxr-xr-x   1 owner owner 0 {} {}"
                   (fTime time, dire)
    Ali.ContentFile file ->
        T.encodeUtf8 $ L.toStrict $
            format "-rw-r--r--   1 owner owner {} {} {}"
                   ( left 10 ' ' $ Shown $ Ali.fileSize file
                   , fTime (Ali.fileLastModified file)
                   , Ali.fileKey file
                   )
  where
    fTime = formatTime defaultTimeLocale "%F %R"

isPlaceholder :: Ali.BucketContent -> Bool
isPlaceholder (Ali.ContentDirectory _ _) = False
isPlaceholder (Ali.ContentFile file) = Ali.fileKey file == ".placeholder"

aliyunService :: Ali.YunConf -> CloudService CloudBackend
aliyunService conf =
    CloudService { listBuckets  = _listBuckets
                 , listObjects  = _listObjects
                 , putBucket    = _putBucket
                 , getObject    = _getObject
                 , putObject    = _putObject
                 , renameObject = _renameObject
                 , removeObject = _removeObject
                 }
  where
    _listBuckets = run conf $
        map (printBucketContent . uncurry Ali.ContentDirectory) . Ali.bucketList <$> Ali.listService

    _listObjects bucket dir = run conf $
        Ali.getBucketContents bucket dir
            C.$= C.filter (not . isPlaceholder)
            C.$= C.map printBucketContent
            C.$$ C.consume

    _putBucket bucket =
        void $ run conf $ Ali.putBucket bucket Nothing

    _getObject bucket file =
        lift (run conf (Ali.getObject bucket file)) >>= mapM_ C.yield . LB.toChunks
    _putObject bucket file = do
        lbs <- LB.fromChunks <$> C.consume
        void $ lift $ run conf $ Ali.putObjectStr bucket file lbs
    _removeObject bucket file =
        void $ run conf $ Ali.deleteObject bucket file

    _renameObject bucket from to = do
        void $ run conf $ Ali.copyObject bucket to from
        void $ run conf $ Ali.deleteObject bucket from
