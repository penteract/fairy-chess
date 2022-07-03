{-# LANGUAGE OverloadedStrings #-}
import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.BaseGame

import Game.Chess.Fairy.Server(wsHandler)

import Data.Text(Text,pack,unpack)
import Text.Read (readMaybe)

import Network.Wai
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.Map as CMap

--type GMap = CMap.Map Text (OngoingGame,MVar ())

html = (hContentType,"text/html")

httpHandler :: Application
httpHandler req resp = do
    let m = requestMethod req
    print (pathInfo req)
    case parseMethod m of
        Right GET -> case pathInfo req of
            [] -> resp$ responseFile ok200 [html] "static/home.html" Nothing
            ["play.html"] -> resp$ responseFile ok200 [html] "static/play.html" Nothing
            ["rule.html"] -> resp$ responseFile ok200 [html] "static/rule.html" Nothing
            _ -> resp$ responseLBS notFound404 [] "Page not found"
        _ -> resp$ responseLBS methodNotAllowed405 [(hAllow,"GET")] ""


--serializeMove = maybe "    " (\ ((a,b),(c,d)) -> concat (map show [a,b,c,d]))
{-
wsHandler :: ServerApp
wsHandler pendingConn = do
    conn <- acceptRequest pendingConn
    --sendTextData conn ("hello"::Text)
    let s = (outerRules.innerRules) center Start emptyState
    withPingThread conn 30 (return ()) (loop conn Nothing s)
    where loop conn lastMove s = do
            sendTextData conn (pack $ drawBoard (board s) ++ fromMaybe "    " lastMove ++ show (result s))
            dat <- unpack <$> receiveData conn
            print dat
            let mpos = mapM (readMaybe.(:[])) dat
            case mpos of
                Just [sx,sy,dx,dy] -> do
                    let mv = Move ((sx,sy),(dx,dy))
                    --putStrLn $ showState (innerRules center mv s)
                    let s' = (outerRules . innerRules ) center mv s
                    putStrLn $ showState s'
                    case result s' of
                        Continue -> loop conn (Just dat) s'
                        _ -> loop conn Nothing s
                _ -> loop conn Nothing s
-}

main :: IO ()
main = do
    putStrLn "starting server"
    games <- CMap.empty
    run 8080 $ websocketsOr defaultConnectionOptions (wsHandler games) httpHandler

debug :: IO ()
debug = do
    let s = emptyState
    let s' = (outerRules.innerRules) center Start s
    putStrLn $ showState s'
    let loop s = do
          mpos <- fmap (mapM (readMaybe.(:[]))) getLine
          case mpos of
            Just [sx,sy,dx,dy] -> do
                let mv = Move ((sx,sy),(dx,dy))
                --putStrLn $ showState (innerRules center mv s)
                let s' = (outerRules . innerRules ) center mv s
                putStrLn $ showState s'
                case result s' of
                    Continue -> loop s'
                    _ -> putStrLn "(Reverting)" >> loop s
            _ -> putStrLn "(Bad input)" >> loop s
    loop s'
    putStrLn "hi"