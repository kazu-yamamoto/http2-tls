module Network.HTTP2.TLS.Client.Settings where

import Data.X509.CertificateStore (CertificateStore)
import Network.Socket

import Network.Control
import Network.HTTP2.Client (
    cacheLimit,
    defaultClientConfig,
 )
import Network.Run.TCP (openClientSocket)
import Network.TLS (
    Information,
    SessionData,
    SessionID,
    SessionManager,
    defaultKeyLogger,
    noSessionManager,
 )

-- Client settings type.
data Settings = Settings
    { settingsKeyLogger :: String -> IO ()
    -- ^ Key logger (TLS and H2)
    --
    -- Applications may wish to set this depending on the SSLKEYLOGFILE environment variable.
    --
    -- Default: do nothing.
    , settingsValidateCert :: Bool
    -- ^ Should we validate TLS certificates? (TLS and H2)
    --
    -- >>> settingsValidateCert defaultSettings
    -- True
    , settingsCAStore :: CertificateStore
    -- ^ Certificate store used for validation. (TLS and H2)
    --
    -- Default: 'mempty'.
    , settingsServerNameOverride :: Maybe HostName
    -- ^ Server name override (H2)
    --
    -- By default, the server name (for TLS SNI) is set based on the
    -- 'Network.HTTP2.Client.authority', corresponding to the HTTP2
    -- @:authority@ pseudo-header. In rare circumstances these two values should
    -- be different (for example in the case of domain fronting);
    -- 'settingsServerNameOverride' can be used to give SNI a different value
    -- than @:authority@.
    , settingsAddrInfoFlags :: [AddrInfoFlag]
    -- ^ Obsoleted.
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
    , settingsSessionManager :: SessionManager
    -- ^ TLS session manager (H2 and TLS)
    --
    -- Default: 'noSessionManager'
    , settingsWantSessionResume :: Maybe (SessionID, SessionData)
    -- ^ Try to resume a TLS session (H2 and TLS)
    --
    -- >>> settingsWantSessionResume defaultSettings
    -- Nothing
    , settingsWantSessionResumeList :: [(SessionID, SessionData)]
    -- ^ Try to resume a TLS session (H2 and TLS).
    -- This takes precedence over 'settingsWantSessionResume'.
    --
    -- >>> settingsWantSessionResumeList defaultSettings
    -- []
    , settingsUseEarlyData :: Bool
    -- ^ Try to use 0-RTT (H2 and TLS)
    --
    -- This is only supported for @tls >= 2.0@.
    --
    -- >>> settingsUseEarlyData defaultSettings
    -- False
    , settingsOpenClientSocket :: AddrInfo -> IO Socket
    -- ^ Function to initialize the server socket (All)
    --
    -- Default: 'openClientSocket'
    , settingsOnServerFinished :: Information -> IO ()
    , settingsPingRateLimit :: Int
    -- ^ Maximum number of pings allowed per second (CVE-2019-9512)
    --
    -- >>> settingsPingRateLimit defaultSettings
    -- 10
    , settingsEmptyFrameRateLimit :: Int
    -- ^ Maximum number of empty data frames allowed per second (CVE-2019-9518)
    --
    -- >>> settingsEmptyFrameRateLimit defaultSettings
    -- 4
    , settingsSettingsRateLimit :: Int
    -- ^ Maximum number of settings frames allowed per second (CVE-2019-9515)
    --
    -- >>> settingsSettingsRateLimit defaultSettings
    -- 4
    , settingsRstRateLimit :: Int
    -- ^ Maximum number of reset frames allowed per second (CVE-2023-44487)
    --
    -- >>> settingsRstRateLimit
    -- 4
    }

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsKeyLogger = defaultKeyLogger
        , settingsValidateCert = True
        , settingsCAStore = mempty
        , settingsServerNameOverride = Nothing
        , settingsAddrInfoFlags = []
        , settingsCacheLimit = cacheLimit defaultClientConfig
        , settingsConcurrentStreams = defaultMaxStreams
        , settingsStreamWindowSize = defaultMaxStreamData
        , settingsConnectionWindowSize = defaultMaxData
        , settingsSessionManager = noSessionManager
        , settingsWantSessionResume = Nothing
        , settingsWantSessionResumeList = []
        , settingsUseEarlyData = False
        , settingsOpenClientSocket = openClientSocket
        , settingsOnServerFinished = \_ -> return ()
        , settingsPingRateLimit = 10
        , settingsEmptyFrameRateLimit = 4
        , settingsSettingsRateLimit = 4
        , settingsRstRateLimit = 4
        }
