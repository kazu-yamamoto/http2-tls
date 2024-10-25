module Network.HTTP2.TLS.Server.Settings where

import Network.Control
import Network.TLS (SessionManager, noSessionManager)

-- Server settings type.
data Settings = Settings
    { settingsTimeout :: Int
    -- ^ Timeout in seconds. (All)
    --
    -- >>> settingsTimeout defaultSettings
    -- 30
    , settingsSendBufferSize :: Int
    -- ^ Send buffer size. (H2 and H2c)
    --
    -- >>> settingsSendBufferSize defaultSettings
    -- 4096
    , settingsSlowlorisSize :: Int
    -- ^ If the size of receiving data is less than or equal,
    --   the timeout is not reset.
    --   (All)
    --
    -- >>> settingsSlowlorisSize defaultSettings
    -- 50
    , settingsReadBufferSize :: Int
    -- ^ When the size of a read buffer is lower than this limit, the buffer is thrown awany (and is eventually freed). Then a new buffer is allocated. (All)
    --
    -- >>> settingsReadBufferSize defaultSettings
    -- 16384
    , settingsReadBufferLowerLimit :: Int
    -- ^  The allocation size for a read buffer.  (All)
    --
    -- >>> settingsReadBufferLowerLimit defaultSettings
    -- 2048
    , settingsKeyLogger :: String -> IO ()
    -- ^ Key logger.
    --
    -- Applications may wish to set this depending on the SSLKEYLOGFILE environment variable. The default is do nothing.
    --
    -- Default: do nothing
    , settingsNumberOfWorkers :: Int
    -- ^ The http2 library now spawns a thread for each connection. Its limit is based on 'settingsConcurrentStreams'.
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
    , settingsEarlyDataSize :: Int
    -- ^ The max size of early data (0-RTT) to be accepted. (H2 and TLS)
    -- 0 means that early data is not accepted.
    --
    -- >>> settingsEarlyDataSize defaultSettings
    -- 0
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

{-# DEPRECATED settingsNumberOfWorkers "This field is meaningless now" #-}

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsTimeout = 30
        , settingsSendBufferSize = 4096
        , settingsSlowlorisSize = 50
        , settingsReadBufferSize = 16384
        , settingsReadBufferLowerLimit = 2048
        , settingsKeyLogger = \_ -> return ()
        , settingsNumberOfWorkers = 8 -- dummy
        , settingsConcurrentStreams = defaultMaxStreams
        , settingsStreamWindowSize = defaultMaxStreamData
        , settingsConnectionWindowSize = defaultMaxData
        , settingsSessionManager = noSessionManager
        , settingsEarlyDataSize = 0
        , settingsPingRateLimit = 10
        , settingsEmptyFrameRateLimit = 4
        , settingsSettingsRateLimit = 4
        , settingsRstRateLimit = 4
        }
