module Network.HTTP2.TLS.Server (
    runH2C,
    Server,
    HostName,
    PortNumber,
) where

import Control.Monad (void)
import Data.ByteString.Char8 ()
import Network.HTTP2.Server
import Network.HTTP2.TLS.Config
import Network.Run.TCP
import Network.Socket (
    HostName,
    PortNumber,
 )
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import qualified UnliftIO.Exception as E

runH2C :: HostName -> PortNumber -> Server -> IO ()
runH2C host port server = do
    runTCPServer (Just host) (show port) $ \sock -> do
        pool <- newBufferPool 2048 16384
        let send = void . NSB.send sock
            recv = receive sock pool
        E.bracket
            (allocConfig 4096 send recv)
            freeConfig
            (\config -> run config server)
