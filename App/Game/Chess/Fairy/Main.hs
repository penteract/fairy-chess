{-#LANGUAGE Haskell2010 #-}
import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.BaseGame

main :: IO ()
main = do
    let s = emptyState
    let s' = (innerRules doNothing) Start s
    print (result s')
    putStrLn (drawBoard $ board s')
    let loop s = do
          mv <- readLn
          let s' = innerRules doNothing (Move mv) s
          putStrLn $ showState s'
          loop s

    loop s'
    putStrLn "hi"