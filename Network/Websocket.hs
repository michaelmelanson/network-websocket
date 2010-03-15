-- | Library for creating Websocket servers.  Some parts cribbed from
-- Jeff Foster's blog post at
-- <http://www.fatvat.co.uk/2010/01/web-sockets-and-haskell.html>
module Network.Websocket( Config(..), WS(..), startServer, send) where

    import Char
    import Control.Concurrent
    import Control.Monad (forever)
    import qualified Network as N
    import qualified Network.Socket as NS
    import System.IO



    -- | Server configuration structure
    data Config = Config {
          -- | The port to bind to
          configPort :: Int,

          -- | The origin URL used in the handshake
          configOrigin :: String,

          -- |The location URL used in the handshake. This must match
          -- the Websocket url that the browsers connect to.
          configLocation  :: String,

          -- | The onopen callback, called when a socket is opened
          configOnOpen    :: WS -> IO (),

          -- | The onmessage callback, called when a message is received
          configOnMessage :: WS -> String -> IO (),

          -- | The onclose callback, called when the connection is closed.
          configOnClose   :: WS -> IO ()
        }

    -- | Connection state structure
    data WS = WS { 
          -- | The server's configuration
          config :: Config,

          -- | The handle of the connected socket
          handle :: Handle
        }
    
    -- |Calls the server's onopen callback, then start reading
    -- |messages. When the connection is terminated, the server's
    -- |onclose callback is called.
    listenLoop ws =
        do onopen ws
           
           (forever $ do
              msg <- readFrame h
              onmessage ws msg) `catch` (\e -> onclose ws)

        where c = config ws
              h = handle ws

              onopen    = configOnOpen c 
              onmessage = configOnMessage c
              onclose   = configOnClose c


    readFrame :: Handle -> IO String
    readFrame h = readUntil h ""
        where readUntil h str =
                  do new <- hGetChar h
                     if new == chr 0
                        then readUntil h ""
                        else if new == chr 255
                                then return str
                                else readUntil h (str ++ [new])

    sendFrame :: Handle -> String -> IO ()
    sendFrame h s = do
      hPutChar h (chr 0)
      hPutStr h s
      hPutChar h (chr 255)

    -- | Send a message to the connected browser.
    send ws = sendFrame (handle ws)

    accept config socket =
        forever $ do
          (h, _, _) <- N.accept socket
          hPutStr h handshake
          hSetBuffering h NoBuffering
          let ws = WS { config = config, handle = h }
          forkIO $ listenLoop ws

        where handshake = "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
                          \Upgrade: WebSocket\r\n\
                          \Connection: Upgrade\r\n\
                          \WebSocket-Origin: " ++ (configOrigin config) ++ "\r\n\
                          \WebSocket-Location: "++ (configLocation config) ++ "\r\n\
                          \WebSocket-Protocol: sample\r\n\r\n"

    -- | Start a websocket server
    startServer config =
        do let port = N.PortNumber $ fromIntegral (configPort config)
           socket <- N.listenOn port
           accept config socket
           NS.sClose socket
           return ()