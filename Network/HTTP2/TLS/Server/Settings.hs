module Network.HTTP2.TLS.Server.Settings where

-- Server settings type.
data Settings = Settings
    { settingsTimeout :: Int
    -- ^ Timeout in seconds. (All)
    , settingsSendBufferSize :: Int
    -- ^ Send buffer size. (H2 and H2c)
    , settingsSlowlorisSize :: Int
    -- ^ If the size of receiving data is less than or equal,
    --   the timeout is not reset.
    --   (All)
    , settingsReadBufferSize :: Int
    -- ^ When the size of a read buffer is lower than this limit, the buffer is thrown awany (and is eventually freed). Then a new buffer is allocated. (All)
    , settingsReadBufferLowerLimit :: Int
    -- ^  The allocation size for a read buffer.  (All)
    , settingsKeyLogger :: String -> IO ()
    -- ^ Key logger (defaults to none)
    --
    -- Applications may wish to set this depending on the SSLKEYLOGFILE environment variable.
    }

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
        }
