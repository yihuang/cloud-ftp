{-# LANGUAGE OverloadedStrings #-}
import qualified Prelude
import BasicPrelude
import qualified Data.Text.Encoding as T
import qualified Network.Aliyun as Ali
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
    aliyunConf <- loadConf "aliyun.conf"
    let serverConf = ServerSettings 8000 HostAny
    runCloudBackend (CloudConf aliyunConf) $ runTCPServer serverConf ftpServer
