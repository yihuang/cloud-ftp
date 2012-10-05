{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           , TypeFamilies
           , RecordWildCards
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

import Control.Monad.Trans.RWS (modify, gets)

import Filesystem.Path.Internal (FilePath(..))
import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.FTP.Utils (encode)
import Network.FTP.Backend (FTPBackend(..))
import Network.FTP.Backend.Cloud.Types
import Network.FTP.Backend.Cloud.Aliyun (aliyunService)

-- | remove path root, and split top directory as bucket name, encode the rest of it.
splitBucket :: FilePath -> Maybe (ByteString, ByteString)
splitBucket p@FilePath{..} =
    case pathDirectories of
        -- 没有路径，则把文件名当唯一的目录
        [] -> fmap (\p -> (T.encodeUtf8 (T.pack p), ""))
                   pathBasename
        (d:ds) -> Just
              ( T.encodeUtf8 (T.pack d)
              , encode $
                  p { pathRoot = Nothing
                    , pathDirectories = ds
                    }
              )

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

    -- split bucket name and path prefix
    list (splitBucket -> Just (bucket, dir)) = do
        ls <- lift $ withService $ \srv -> listObjects srv bucket dir
        yield $ S.intercalate "\n" ls

    -- root directory is bucket list
    list _ = do
        ls <- lift $ withService listBuckets
        yield $ S.intercalate "\n" ls

    nlst _ =
        return ()

    mkd (splitBucket -> Just (bucket, dir)) =
        if S.null dir
          -- top directory, create bucket
          then withService $ \srv -> putBucket srv bucket
          -- sub directory, create dir file
          else withService $ \srv -> yield "" $$ putObject srv bucket (dir ++ "/.placeholder")
    mkd _ = fail "Please specify a directory."

    dele (splitBucket -> Just (bucket, file)) =
        withService $ \srv -> removeObject srv bucket file
    dele _ = fail "don't support file in root directory"

    rename (splitBucket -> Just (bucket1, file1))
           (splitBucket -> Just (bucket2, file2)) = do
        when (bucket1/=bucket2) $
            fail "only support rename within same bucket."
        void $ withService $ \srv -> renameObject srv bucket1 file1 file2
    rename _ _ = fail "don't support file in root directory"

    rmd _ = fail "not implemented."

    download (splitBucket -> Just (bucket, file)) = do
        srv <- lift $ CloudBackend (gets stService)
        getObject srv bucket file
    download _ = fail "don't support file in root directory"

    upload   (splitBucket -> Just (bucket, file)) = do
        srv <- lift $ CloudBackend (gets stService)
        putObject srv bucket file
    upload _ = fail "don't support file in root directory"
