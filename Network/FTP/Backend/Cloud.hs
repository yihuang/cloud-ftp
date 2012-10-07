{-# LANGUAGE OverloadedStrings
           , ViewPatterns
           , TypeFamilies
           , RecordWildCards
           #-}
module Network.FTP.Backend.Cloud
  ( CloudConf(..)
  , CloudBackend(..)
  , runCloudBackend
  , loadCloudConf
  ) where

{-|
 - Cloud storage based backend.
 -}

import qualified Prelude as P
import BasicPrelude
import Control.Monad.Trans.RWS (modify, gets, asks)
import Filesystem.Path.Internal (FilePath(..))

import Data.Maybe (maybeToList)
import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as Ini

import Network.Aliyun (YunConf(YunConf))
import Network.FTP.Utils (encode)
import Network.FTP.Backend (FTPBackend(..))
import Network.FTP.Backend.Cloud.Types
import Network.FTP.Backend.Cloud.Aliyun (aliyunService)

-- | load configuration from ini file
loadCloudConf :: String -> IO CloudConf
loadCloudConf filepath = do
    content <- P.readFile filepath
    ini <- either (fail . ("parse failed:"++) . P.show) return $ Ini.parse content
    let users = HM.fromList $ do
        (name, section) <- M.toList ini
        user <- maybeToList $
            CloudUser (S.pack name)
                  <$> (S.pack <$> M.lookup "password" section)
                  <*> (S.pack <$> M.lookup "service" section)
                  <*> (S.pack <$> M.lookup "host" section)
                  <*> (S.pack <$> M.lookup "identity" section)
                  <*> (S.pack <$> M.lookup "key" section)
        return (userName user, user)
    return $ CloudConf users

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

    authenticate name pass = do
        -- lookup user
        users <- CloudBackend $ asks (cloudUsers . fst)
        case HM.lookup name users of
            Nothing -> return Nothing
            Just u -> do
                if pass==userPass u
                  then do
                    let aliConf = YunConf (userHost u) (userIdentity u) (userKey u)
                    CloudBackend $ modify $ \st -> st { stService = aliyunService aliConf }
                    return (Just name)
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
