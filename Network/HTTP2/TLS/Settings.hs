module Network.HTTP2.TLS.Settings where

-- Settings type.
data Settings = Settings
    { settingsTimeout :: Int
    -- ^ Timeout in seconds. (All)
    , settingsSendBufferSize :: Int
    -- ^ Send buffer size. (H2 and H2c)
    , settingsSlowlorisSize :: Int
    -- ^ If the size of receiving data is less than or equal,
    --   the timeout is not reset.
    --   (All)
    , settingReadBufferSize :: Int
    -- ^ When the size of a read buffer is lower than this limit, the buffer is thrown awany (and is eventually freed). Then a new buffer is allocated. (All)
    , settingReadBufferLowerLimit :: Int
    -- ^  The allocation size for a read buffer.  (All)
    } deriving (Eq, Show)

-- | Default settings.
--
-- >>> defaultSettings
-- Settings {settingsTimeout = 30, settingsSendBufferSize = 4096, settingsSlowlorisSize = 50, settingReadBufferSize = 16384, settingReadBufferLowerLimit = 2048}
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsTimeout = 30
        , settingsSendBufferSize = 4096
        , settingsSlowlorisSize = 50
        , settingReadBufferSize = 16384
        , settingReadBufferLowerLimit = 2048
        }
