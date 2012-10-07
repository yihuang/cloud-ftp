{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
import qualified Prelude as P
import BasicPrelude
import Filesystem.Path.CurrentOS (decodeString)
import System.Environment (getArgs)
import qualified Data.Text.Encoding as T
import qualified Network.Aliyun as Ali
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference(HostAny), serverNeedLocalAddr)
import Network.HTTP.Conduit
import Network.FTP.Server
import Network.FTP.Backend.Cloud

{-
 - Before run test.hs, create a file named "config" in current directory,
 - which contains two lines, the first line is identity, second line is secret key.
 -}

loadConf :: FilePath -> IO Ali.YunConf
loadConf path = do
    (ident : key : _) <- lines <$> readFile path
    return $ Ali.YunConf "storage.aliyun.com" (T.encodeUtf8 ident) (T.encodeUtf8 key)

main = do
    [P.read -> port, path] <- getArgs
    aliyunConf <- loadConf (decodeString path)
    let serverConf = (serverSettings port HostAny){serverNeedLocalAddr=True}
    runCloudBackend (CloudConf aliyunConf) $ runTCPServer serverConf ftpServer
