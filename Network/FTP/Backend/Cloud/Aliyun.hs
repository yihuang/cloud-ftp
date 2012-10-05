{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Network.FTP.Backend.Cloud.Aliyun where

import qualified Prelude as P
import BasicPrelude
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Text.Format
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Network.Aliyun as Ali
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS
import Network.FTP.Utils (encode)
import Network.FTP.Backend.Cloud.Types

run :: Ali.Yun a -> CloudBackend a
run yun = do
    man <- CloudBackend (asks snd)
    conf <- CloudBackend (asks (aliyunConf . fst))
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

aliyunService :: CloudService CloudBackend
aliyunService =
    CloudService { listBuckets = listBuckets
                 , listObjects = listObjects
                 , putBucket   = putBucket
                 , getObject   = getObject
                 , putObject   = putObject
                 , renameObject = renameObject
                 , removeObject = removeObject
                 }
  where
    listBuckets = run $
        map (printBucketContent . uncurry Ali.ContentDirectory) . Ali.bucketList <$> Ali.listService

    listObjects bucket dir = run $
        Ali.getBucketContents bucket dir
            C.$= C.filter (not . isPlaceholder)
            C.$= C.map printBucketContent
            C.$$ C.consume

    putBucket bucket =
        void $ run $ Ali.putBucket bucket Nothing

    getObject bucket file =
        lift (run (Ali.getObject bucket (T.encodeUtf8 file))) >>= mapM_ C.yield . LB.toChunks
    putObject bucket file = do
        lbs <- LB.fromChunks <$> C.consume
        lift $ run $ Ali.putObjectStr bucket (T.encodeUtf8 file) lbs
        return ()
    removeObject bucket file =
        void $ run $ Ali.deleteObject bucket (T.encodeUtf8 file)

    renameObject bucket from to = do
        run $ Ali.copyObject bucket (T.encodeUtf8 to) (T.encodeUtf8 from)
        run $ Ali.deleteObject bucket (T.encodeUtf8 from)
        return ()
