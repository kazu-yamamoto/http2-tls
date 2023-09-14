{-# LANGUAGE CPP #-}

module Network.HTTP2.TLS.Internal (
    module Network.HTTP2.TLS.IO,
    gclose,
) where

import Network.Socket

import Network.HTTP2.TLS.IO

gclose :: Socket -> IO ()
#if MIN_VERSION_network(3,1,1)
gclose sock = gracefulClose sock 5000
#else
gclose = close
#endif
