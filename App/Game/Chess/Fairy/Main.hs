{-# LANGUAGE OverloadedStrings #-}
import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.BaseGame

import Text.Read (readMaybe)
import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as CMap

import Network.Wai
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp (run)

--type GMap = CMap.Map Text (OngoingGame,MVar ())

html = (hContentType,"text/html")

app :: Application
app req resp = do
    let m = requestMethod req
    case parseMethod m of
        Right GET -> resp$ responseFile ok200 [html] "static/play.html" Nothing
        -- Right POST -> onPost games req resp
        _ -> resp$ responseLBS methodNotAllowed405 [(hAllow,"GET, POST")] ""

main :: IO ()
main = putStrLn "starting server" >> run 8080 app

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