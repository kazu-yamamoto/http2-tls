module Network.HTTP2.TLS.Client.Settings where

import Data.X509.CertificateStore (CertificateStore)
import Network.Socket

import Network.Control
import Network.HTTP2.Client (
    cacheLimit,
    defaultClientConfig,
 )

-- Client settings type.
data Settings = Settings
    { settingsKeyLogger :: String -> IO ()
    -- ^ Key logger (TLS and H2)
    --
    -- Applications may wish to set this depending on the SSLKEYLOGFILE environment variable. Default is do nothing.
    , settingsValidateCert :: Bool
    -- ^ Should we validate TLS certificates? (TLS and H2)
    --
    -- >>> settingsValidateCert defaultSettings
    -- True
    , settingsCAStore :: CertificateStore
    -- ^ Certificate store used for validation. The default is 'mempty'. (TLS and H2)
    , settingsAddrInfoFlags :: [AddrInfoFlag]
    -- ^ Flags that control the querying behaviour of @getAddrInfo@. (TLS and H2)
    --
    -- >>> settingsAddrInfoFlags defaultSettings
    -- []
    , settingsCacheLimit :: Int
    -- ^ How many pushed responses are contained in the cache (H2 and H2c)
    --
    -- >>> settingsCacheLimits defaultSettings
    -- 64
    , settingsConcurrentStreams :: Int
    -- ^ The maximum number of incoming streams on the net (H2 and H2c)
    --
    -- >>> settingsConcurrentStreams defaultSettings
    -- 64
    , settingsStreamWindowSize :: Int
    -- ^ The window size of incoming streams (H2 and H2c)
    --
    -- >>> settingsStreamWindowSize defaultSettings
    -- 262144
    , settingsConnectionWindowSize :: Int
    -- ^ The window size of a connection (H2 and H2c)
    --
    -- >>> settingsConnectionWindowSize defaultSettings
    -- 1048575
    }

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsKeyLogger = \_ -> return ()
        , settingsValidateCert = True
        , settingsCAStore = mempty
        , settingsAddrInfoFlags = []
        , settingsCacheLimit = cacheLimit defaultClientConfig
        , settingsConcurrentStreams = defaultMaxStreams
        , settingsStreamWindowSize = defaultMaxStreamData
        , settingsConnectionWindowSize = defaultMaxData
        }
