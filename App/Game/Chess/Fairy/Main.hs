{-#LANGUAGE Haskell2010 #-}
import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.BaseGame
import Text.Read (readMaybe)

main :: IO ()
main = do
    let s = emptyState
    let s' = (rPawnMoves.rSetLayout) center Start s
    putStrLn $ showState s'
    let loop s = do
          mpos <- fmap (mapM (readMaybe.(:[]))) getLine
          case mpos of
            Just [sx,sy,dx,dy] -> do
                let mv = Move ((sx,sy),(dx,dy))
                putStrLn $ showState (rPawnMoves center mv s)
                let s' = (outerRules . rPawnMoves ) center mv s
                putStrLn $ showState s'
                case result s' of
                    Continue -> loop s'
                    _ -> putStrLn "(Reverting)" >> loop s
            _ -> putStrLn "(Bad input)" >> loop s

    loop s'
    putStrLn "hi"