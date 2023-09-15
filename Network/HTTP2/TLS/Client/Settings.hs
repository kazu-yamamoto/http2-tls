module Network.HTTP2.TLS.Client.Settings where

import Data.X509.CertificateStore (CertificateStore)
import Network.Socket

-- Client settings type.
data Settings = Settings
    { settingsKeyLogger :: String -> IO ()
    -- ^ Key logger
    --
    -- Applications may wish to set this depending on the SSLKEYLOGFILE environment variable. Default is do nothing.

    , settingsValidateCert :: Bool
    -- ^ Should we validate TLS certificates?
    --
    -- >>> settingsValidateCert defaultSettings
    -- True

    , settingsCAStore :: CertificateStore
    -- ^ Certificate store used for validation. The default is 'mempty'.

    , settingsAddrInfoFlags :: [AddrInfoFlag]
    -- ^ Flags that control the querying behaviour of @getAddrInfo@.
    --
    -- >>> settingsAddrInfoFlags defaultSettings
    -- []
    }

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsKeyLogger     = \_ -> return ()
        , settingsValidateCert  = True
        , settingsCAStore       = mempty
        , settingsAddrInfoFlags = []
        }


