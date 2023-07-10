module Network.HTTP2.TLS.Settings where

data Settings = Settings
    { settingsTimeout :: Int
    -- ^ Seconds
    , settingsSendBufferSize :: Int
    -- ^ HTTP/2
    , settingsSlowlorisSize :: Int
    , settingReadBufferSize :: Int
    -- ^ TCP
    , settingReadBufferLowerLimit :: Int
    -- ^ TCP
    }

defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsTimeout = 30
        , settingsSendBufferSize = 4096
        , settingsSlowlorisSize = 50
        , settingReadBufferSize = 16384
        , settingReadBufferLowerLimit = 2048
        }
