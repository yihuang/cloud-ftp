{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           , TypeFamilies
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

import Control.Monad.Trans.RWS

import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Process as C
import qualified Data.Text.Encoding as T
import Network.HTTP.Conduit

import Network.FTP.Utils (dropHeadingPathSeparator, encode, decode)
import Network.FTP.Backend (FTPBackend(..))
import Network.FTP.Backend.Cloud.Types
import Network.FTP.Backend.Cloud.Aliyun (aliyunService)

dropHeading :: ByteString -> ByteString
dropHeading s =
    case S.uncons s of
        Just ('/', s') -> s'
        _         -> s

instance FTPBackend CloudBackend where
    type UserId CloudBackend = ByteString

    ftplog = liftIO . S.putStrLn

    authenticate user pass =
        if user==pass
          then do
            when (user=="aliyun") $
              CloudBackend $ modify $ \st -> st { stService = aliyunService }
            return (Just user)
          else return Nothing

    -- list buckets for root
    list "/" = do
        lines <- lift $ withService $ listBuckets
        yield $ S.intercalate "\n" lines

    -- split bucket name and path prefix
    list (dropHeadingPathSeparator -> path) = do
        lines <- lift $ withService $ \srv -> listObjects srv bucket (T.decodeUtf8 dir)
        yield $ S.intercalate "\n" lines
      where
        (bucket, dropHeading -> dir) = S.span (/='/') (encode path)

    list _ = fail "only support one level directory."

    nlst dir =
        return ()

    mkd (dropHeadingPathSeparator -> path) = do
        if S.null dir
          -- top directory, create bucket
          then withService $ \srv -> putBucket srv bucket
          -- sub directory, create dir file
          else withService $ \srv -> yield "" $$ putObject srv bucket (T.decodeUtf8 dir ++ "/.placeholder")
      where
        (bucket, dropHeading -> dir) = S.span (/='/') (encode path)

    dele (dropHeadingPathSeparator -> path) =
        withService $ \srv -> removeObject srv bucket (T.decodeUtf8 file)
      where
        (bucket, file) = S.span (/='/') (encode path)

    rename fname tname = do
        when (bucket1/=bucket2) $
            fail "only support rename within same top directory."
        withService $ \srv -> renameObject srv bucket1 (T.decodeUtf8 file1) (T.decodeUtf8 file2)
        return ()
      where
        (bucket1, dropHeading -> file1) = S.span (/='/') (encode $ dropHeadingPathSeparator fname)
        (bucket2, dropHeading -> file2) = S.span (/='/') (encode $ dropHeadingPathSeparator tname)

    rmd dir = do
        return ()

    download (dropHeadingPathSeparator -> path) = do
        srv <- lift $ CloudBackend (gets stService)
        getObject srv bucket (T.decodeUtf8 file)
      where
        (bucket, dropHeading -> file) = S.span (/='/') (encode path)

    upload   (dropHeadingPathSeparator -> path) = do
        srv <- lift $ CloudBackend (gets stService)
        putObject srv bucket (T.decodeUtf8 file)
      where
        (bucket, dropHeading -> file) = S.span (/='/') (encode path)
