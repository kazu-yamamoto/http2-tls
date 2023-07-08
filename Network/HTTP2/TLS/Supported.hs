module Network.HTTP2.TLS.Supported where

import Data.Default.Class (def)
import Network.TLS hiding (HostName)
import Network.TLS.Extra

strongSupported :: Supported
strongSupported =
    def -- TLS.Supported
        { supportedVersions = [TLS13, TLS12]
        , supportedCiphers = ciphersuite_strong
        , supportedCompressions = [nullCompression]
        , supportedSecureRenegotiation = True
        , supportedClientInitiatedRenegotiation = False
        , supportedSession = True
        , supportedFallbackScsv = True
        , supportedGroups = [X25519, P256, P384]
        }
