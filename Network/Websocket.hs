{-# LANGUAGE ScopedTypeVariables #-}

-- | Library for creating Websocket servers.  Some parts cribbed from
-- Jeff Foster's blog post at
-- <http://www.fatvat.co.uk/2010/01/web-sockets-and-haskell.html>
module Network.Websocket( Config(..), ConfigRestriction(..), WS(..),
                          startServer, send ) where

    import Char (chr)
    import Control.Concurrent
    import Control.Exception hiding (catch)
    import Control.Monad
    import Data.Char (isSpace)
    import Data.Maybe
    import qualified Network as N
    import qualified Network.Socket as NS
    import Network.Web.HTTP
    import Network.URI
    import Network.Web.Server
    import System.IO


    trim = f . f
        where f = reverse.dropWhile isSpace

    whenM b f = b >>= \b' -> when b' f

    instance Show Request where
        show req = let method = show $ reqMethod req
                       uri    = show $ reqURI req
                       fields = show $ reqFields req
                   in method ++ " " ++ uri ++ "\n" ++ fields

    data ConfigRestriction  = Any | Only [String]
    restrictionValid x Any = True
    restrictionValid x (Only xs) = elem x xs
                                   
    instance Eq Status


    -- | Server configuration structure
    data Config = Config {
          -- | The port to bind to
          configPort :: Int,

          -- | The origin URL used in the handshake
          configOrigins :: ConfigRestriction,

          -- |The location URL used in the handshake. This must match
          -- the Websocket url that the browsers connect to.
          configDomains  :: ConfigRestriction,

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
          wsConfig :: Config,

          -- | The handle of the connected socket
          wsHandle :: Handle
        }




    -- | Calls the server's onopen callback, then start reading
    -- messages. When the connection is terminated, the server's
    -- onclose callback is called.
    listenLoop ws =
        do onopen ws

           (forever $ do
              msg <- readFrame h
              onmessage ws msg)
                      `catch`
                      (\e -> onclose ws)

           return ()

        where c = wsConfig ws
              h = wsHandle ws

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
      putStrLn $ "Sending message: " ++ s
      hPutChar h (chr 0)
      hPutStr  h s
      hPutChar h (chr 255)
      hFlush h

    -- | Send a message to the connected browser.
    send ws = sendFrame (wsHandle ws)

    parseRequest req = do
      upgrade  <- lookupField (FkOther "Upgrade") req
      origin   <- lookupField (FkOther "Origin") req
      host     <- lookupField FkHost req
      hostURI  <- parseURI ("ws://" ++ host ++ "/")
      hostAuth <- uriAuthority hostURI
      let domain = uriRegName hostAuth

      return (upgrade, origin, domain)

    doWebSocket socket f =
        bracket (do (h :: Handle, _, _) <- N.accept socket
                    maybeReq <- receive h
                    return (h, maybeReq))

                (\(h,_) -> hClose h)

                (\(h, maybeReq) ->
                     case maybeReq of
                       Nothing -> putStrLn "Got bad request"
                       Just req -> f h req)

    sendHandshake h origin location = hPutStr h handshake >> hFlush h
        where handshake = "HTTP/1.1 101 Web Socket Protocol Handshake\r\n\
                          \Upgrade: WebSocket\r\n\
                          \Connection: Upgrade\r\n\
                          \WebSocket-Origin: " ++ origin ++ "\r\n\
                          \WebSocket-Location: "++ show location ++ "\r\n\
                          \WebSocket-Protocol: sample\r\n\r\n"


    accept config socket =
        forever $ doWebSocket socket $ \h req ->
            do let (upgrade, origin, hostDomain) = case parseRequest req of
                                                     Nothing -> throw (userError "Invalid request")
                                                     Just a -> a
                   location = (reqURI req) { uriScheme = "ws:" }
                   ws = WS { wsConfig = config, wsHandle = h }

               return $ assert (upgrade == "WebSocket") ()
               return $ assert (restrictionValid origin (configOrigins config)) ()
               return $ assert (restrictionValid hostDomain (configDomains config)) ()

               sendHandshake h origin location

               onOpen ws
               (forever $ do msg <- readFrame h
                             onMessage ws msg) `catch` (\e -> onClose ws)

        where onOpen    = configOnOpen config
              onMessage = configOnMessage config
              onClose   = configOnClose config


    -- | Start a websocket server
    startServer config =
        do let port = N.PortNumber $ fromIntegral (configPort config)
           socket <- N.listenOn port
           accept config socket
           NS.sClose socket
           return ()
