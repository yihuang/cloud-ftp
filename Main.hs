import Network.FTP.Server
import Network.FTP.Backend.Cloud

main = do
    let serverConf = ServerSettings 8000 HostAny
    runCloudBackend $ runTCPServer serverConf ftpServer
