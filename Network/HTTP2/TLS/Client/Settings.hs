module Network.HTTP2.TLS.Client.Settings where

import Data.X509.CertificateStore (CertificateStore)
import Network.Socket

-- Client settings type.
data Settings = Settings
    { settingsKeyLogger :: String -> IO ()
    -- ^ Key logger (defaults to none)
    --
    -- Applications may wish to set this depending on the SSLKEYLOGFILE environment variable.

    , settingsValidateCert :: Bool
    -- ^ Should we validate TLS certificates?
    --
    -- Defaults to @False@

    , settingsCAStore :: CertificateStore
    -- ^ Certificate store used for validation
    --
    -- Defaults to @mempty@

    , settingsAddrInfoFlags :: [AddrInfoFlag]
    -- ^ Flags that control the querying behaviour of @getAddrInfo@.
    --
    -- Defaults to @[AI_ADDRCONFIG, AI_NUMERICHOST, AI_NUMERICSERV]@
    }

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsKeyLogger     = \_ -> return ()
        , settingsValidateCert  = False
        , settingsCAStore       = mempty
        , settingsAddrInfoFlags = [AI_ADDRCONFIG, AI_NUMERICHOST, AI_NUMERICSERV]
        }


