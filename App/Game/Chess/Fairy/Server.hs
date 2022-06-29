{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Chess.Fairy.Server(wsHandler) where

import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.BaseGame

import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as CMap
import Data.IORef
import Network.WebSockets

import Data.Text.Lazy (Text,pack,unpack)
import qualified Data.ByteString.Char8 as B
import Data.Bits(shift)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable(toList)
import qualified Data.Foldable as F
import Control.Monad (forever)
import Control.Exception (finally)
import Text.Read (readMaybe)


-- A structure for keeping track of all games currently being played
type GMap = CMap.Map B.ByteString (MVar OngoingGame)

wsHandler :: GMap -> ServerApp
wsHandler games pending = do
    let path = requestPath (pendingRequest pending)
    print path
    let gameID = do
          q <- B.split '&' <$> B.stripPrefix "/play?" path
          foldr ((<>) . B.stripPrefix "game=") Nothing q
    maybe (rejectRequest pending "invalid url")
      (\ gid -> do
        gvar <- newGame
        inserted <- CMap.insertIfAbsent gid gvar games
        Just gvar' <- if inserted then return (Just gvar) else CMap.lookup gid games -- If at some point games are removed, throwing here is acceptable.
        c <- acceptRequest pending
        handleJoin c gvar')
      gameID

newGame :: IO (MVar OngoingGame)
newGame = do
    v <- newEmptyMVar
    putMVar v (OG emptyState [] (Turn One 0 0) [] Nothing Nothing (Leaf Nothing))
    return v


data PlayerNum = One | Two deriving (Eq,Show)
otherNum :: PlayerNum -> PlayerNum
otherNum One = Two
otherNum Two = One
data TurnInfo = Turn {plNum :: PlayerNum, tEnd::Int, oppRemaining::Int} | Rule PlayerNum deriving Show
data OngoingGame = OG {gs :: GameState, history :: [String], tinfo :: TurnInfo, rules :: [Rule],
    playerOne :: Maybe (Text,Maybe Connection),  playerTwo :: Maybe (Text, Maybe Connection), observers :: Tree Connection
    }

instance Show OngoingGame where
    show OG{playerOne,playerTwo} = "Game "++show playerOne++ ";" ++ show playerTwo

instance Show Connection where
    show conn = "<conn>"

getPlayer :: PlayerNum -> OngoingGame -> Maybe (Text,Maybe Connection)
getPlayer One = playerOne
getPlayer Two = playerTwo

setPlayer :: PlayerNum -> Maybe (Text,Maybe Connection) -> OngoingGame -> OngoingGame
setPlayer One p og = og{playerOne=p}
setPlayer Two p og = og{playerTwo=p}

handleJoin :: Connection -> MVar OngoingGame -> IO ()
handleJoin c gvar = do
    putStrLn "handling join"
    secret <- receiveData c:: IO Text
    print secret
    og <- takeMVar gvar
    print og
    let tryAddPlayer :: PlayerNum -> Maybe (IO ())
        tryAddPlayer n = case getPlayer n og of
          Nothing -> Just (play c n (setPlayer n (Just (secret, Just c)) og) gvar)
          Just (sec,Nothing) -> if sec==secret then Just (play c n (setPlayer n (Just (secret, Just c)) og) gvar) else Nothing
          Just (sec,Just conn) -> if sec==secret then Just (putMVar gvar og) else Nothing
        addListener = let (ns,n) = addObserver c og in putMVar gvar ns  >> removeOnClose c n gvar
    fromMaybe addListener (tryAddPlayer One <> tryAddPlayer Two)

addObserver :: Connection -> OngoingGame -> (OngoingGame, Int)
addObserver conn og@OG{observers=os} = let (t',n) = add conn os in (og{observers=t'} , n)

removeOnClose :: Connection -> Int -> MVar OngoingGame -> IO ()
removeOnClose conn n gvar = forever (receive conn) `finally` do
    og@OG{observers=os} <- takeMVar gvar
    let os' = remove n os
    putMVar gvar og{observers=os'}


play :: Connection -> PlayerNum -> OngoingGame -> MVar OngoingGame -> IO ()
play conn pn og gvar = do
    putStrLn ("starting play"++show pn)
    putMVar gvar og
    forever (do
        msg <- unpack <$> receiveData conn
        print msg
        case parseMove msg of
            Just mv -> handleMove conn pn mv gvar -- TODO: handle winning and adding a new rule
            Nothing -> close conn)
        `finally` do
      og <- takeMVar gvar
      case getPlayer pn og of
        Nothing -> error "The player has joined the game, so this should be impossible"
        Just (s,_) -> putMVar gvar (setPlayer pn (Just (s,Nothing)) og)

parseMove :: String -> Maybe Move
parseMove dat =
    let mpos = mapM (readMaybe.(:[])) dat in
    case mpos of
        Just [sx,sy,dx,dy] -> Just ((sx,sy),(dx,dy))
        _ -> Nothing

close :: Connection -> IO ()
close conn = do
    sendClose conn (""::Text)
    forever (receive conn)


handleMove :: Connection -> PlayerNum -> Move -> MVar OngoingGame -> IO ()
handleMove conn pn mv gvar = do
    {-TODO: consider finding a way to make observers joining independent of the players MVar
    mog <- tryTakeMVar mv
    case mog of
        Nothing -> err "other move being processed"
        -}
    og <- takeMVar gvar
    case tinfo og of
      Turn pn' _ _ | pn==pn' -> let gs' = foldr ($) center (rules og) (Move mv) (gs og)
                                    og' = og{gs=gs'} in case result gs' of
        Illegal -> publishMove og ('I':show mv) gvar
        Win -> publishMove og' ('W':show mv) gvar
        Draw -> publishMove og' ('D':show mv) gvar
        Continue -> publishMove og'{tinfo = Turn (otherNum pn) 0 0} ('C':show mv) gvar
                  | otherwise -> sendTextData conn ("ErrTurn"::Text)
      Rule pn -> sendTextData conn ("ErrRule"::Text)

publishMove :: OngoingGame -> String -> MVar OngoingGame -> IO ()
publishMove og s var = do
    broadcast og (s ++ showState (gs og))
    putMVar var og

broadcast :: OngoingGame -> [Char] -> IO ()
broadcast OG{playerOne,playerTwo,observers} s = let txt = pack s in
    mapM_ (flip sendTextData txt) (([playerOne, playerTwo]>>=(concat.(F.toList.snd<$>). F.toList) ) ++ treeToList observers)





-- depth, capacity, left, right
data Tree a = Branch Int Int (Tree a) (Tree a) | Leaf (Maybe a) deriving (Foldable,Show)

hasSpace :: Tree a -> Bool
hasSpace (Leaf (Just _)) = False
hasSpace (Branch _ 0 _ _) = False
hasSpace _ = True

depth (Leaf _) = 0
depth (Branch d _ _ _) = d

add :: a -> Tree a -> (Tree a,Int)
add x (Leaf Nothing) = (Leaf (Just x), 0)
add x (Branch d c l r) | c/=0 = if hasSpace l
    then let (l',n) = add x l in (Branch d (c-1) l' r, n)
    else let (r',n) = add x l in (Branch d (c-1) l r', n+(1`shift`(d-1)))
add x t = let d = depth t in (Branch (d + 1) 0 t (fst $ add x (emptyTrees !! d)), 1 `shift` d)


remove :: Int -> Tree a -> Tree a
remove n (Leaf _) = Leaf Nothing
remove n (Branch d _ l r) = if n>=(1`shift`(d-1)) then remove (n-(1`shift`d-1)) r else remove n l

emptyTrees = Leaf Nothing : map (\t -> let d = depth t + 1 in Branch d (1`shift`d) t t) emptyTrees

treeToList :: Tree a -> [a]
treeToList = Data.Foldable.toList