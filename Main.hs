{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
import qualified Prelude as P
import BasicPrelude
import Filesystem.Path.CurrentOS (decodeString)
import System.Environment (getArgs)

import qualified Data.Text.Encoding as T
import qualified Data.Ini.Reader as Ini
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference(HostAny), serverNeedLocalAddr)

import Network.HTTP.Conduit
import qualified Network.Aliyun as Ali
import Network.FTP.Server
import Network.FTP.Backend.Cloud

main = do
    [P.read -> port, path] <- getArgs
    conf <- loadCloudConf path
    let serverConf = (serverSettings port HostAny){serverNeedLocalAddr=True}
    runCloudBackend conf $ runTCPServer serverConf ftpServer
