{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module Game.Chess.Fairy.Server(wsHandler) where

import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.BaseGame

import Control.Concurrent.MVar
import qualified Control.Concurrent.Map as CMap
import Data.IORef
import Network.WebSockets

import Data.Text.Lazy (Text,pack,unpack, cons)
import qualified Data.ByteString.Char8 as B
import Data.Bits(shift)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Foldable(toList)
import qualified Data.Foldable as F
import Control.Monad (forever)
import Control.Exception (finally, evaluate, mask, onException)
import Text.Read (readMaybe)
import Control.Arrow(second)

import Language.Haskell.Interpreter

 -- Data structures for a game in progress

data PlayerNum = One | Two deriving (Eq,Show)
otherNum :: PlayerNum -> PlayerNum
otherNum One = Two
otherNum Two = One
data TurnInfo = Turn {plNum :: PlayerNum, tEnd::Int, oppRemaining::Int} | Rule PlayerNum deriving Show

data OngoingGame = OG {gs :: GameState, history :: [String], tinfo :: TurnInfo, rules :: [Rule],
    playerOne :: Maybe (Text,Tree Connection),  playerTwo :: Maybe (Text, Tree Connection), observers :: Tree Connection
    }

newGame :: IO (MVar OngoingGame)
newGame = do
    v <- newEmptyMVar
    putMVar v (OG emptyState [] (Turn One 0 0) [] Nothing Nothing (Leaf Nothing))
    return v

instance Show OngoingGame where
    show OG{playerOne,playerTwo,tinfo} = "Game "++show playerOne++ ";" ++ show playerTwo ++ "@"++ show tinfo

instance Show Connection where
    show conn = "<conn>"

getPlayers :: PlayerNum -> OngoingGame -> Maybe (Text, Tree Connection)
getPlayers One = playerOne
getPlayers Two = playerTwo

setPlayers :: PlayerNum -> Maybe (Text, Tree Connection) -> OngoingGame -> OngoingGame
setPlayers One p og = og{playerOne=p}
setPlayers Two p og = og{playerTwo=p}


-- A structure for keeping track of all games currently being played
type GMap = CMap.Map B.ByteString (MVar OngoingGame)


-- |Messages sent by the server
data ServerMsg = Init ExpectedAction GameState -- Start a game (including restart after a rule has been added)
  | NewState Move ExpectedAction GameState -- 
  | Attempt Move -- A failed move
  | Err Text

data ExpectedAction = YourMove | YourRule | Wait | WaitRule | Observe

-- |Messages sent by the client
data ClientMsg = MakeMove Move| RandomMove | NewRule String

-- Main game logic

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
        withPingThread c 30 (return ()) (handleJoin c gvar'))
      gameID

handleJoin :: Connection -> MVar OngoingGame -> IO ()
handleJoin c gvar = do
    putStrLn "handling join"
    secret <- receiveData c:: IO Text
    print secret
    og <- takeMVar gvar
    print og
    let tryAddPlayer :: PlayerNum -> Maybe (IO ())
        tryAddPlayer pn = case getPlayers pn og of
          Nothing -> Just (play c 0 pn (setPlayers pn (Just (secret, Leaf (Just c))) og) gvar)
          Just (sec,tree) -> if sec==secret then
            let (t',n) = add c tree in
                Just (play c n pn (setPlayers pn (Just (secret, t')) og) gvar)
            else Nothing
        addListener = let (ns,n) = addObserver c og in putMVar gvar ns  >> removeOnClose c n gvar
    fromMaybe addListener (tryAddPlayer One <> (if isNothing (playerTwo og) then Just (startgame (secret,c) og gvar) else tryAddPlayer Two))

startgame :: (Text,Connection) -> OngoingGame -> MVar OngoingGame -> IO ()
startgame (secret,conn) og@OG{playerOne=Just _, playerTwo=Nothing} gvar = do
    let og' = og{playerTwo=Just (secret,Leaf (Just conn)), gs= applyEvent Start og}
    sendInitMsg og'
    play conn 0 Two og' gvar
startgame _ _ _ = error "Trying to start a game that has already started or is not ready to start"

play :: Connection -> Int -> PlayerNum -> OngoingGame -> MVar OngoingGame -> IO ()
play conn n pn og gvar = do
    putStrLn ("starting play"++show pn)
    sendWelcomeMsg og pn conn
    putMVar gvar og
    forever (do
        msg <- unpack <$> receiveData conn
        print msg
        case parseMsg msg of
            Just (MakeMove mv) ->handleGameSafely (handleMove conn pn mv) gvar
            Just (NewRule r) -> handleGameSafely (handleRule conn pn r) gvar
            Nothing -> putStrLn "unable to parse" >> close conn)
        `finally` do
      putStrLn ("Player "++show pn++" has left")
      og <- takeMVar gvar
      case getPlayers pn og of
        Nothing -> error "The player has joined the game, so this should be impossible, but it looks like something removed them"
        Just (s,t) -> putMVar gvar (setPlayers pn (Just (s,remove n t)) og)

-- Message handlers

-- |'bracket' doesn't do exactly what I want, but this serves a similar purpose
handleGameSafely :: (OngoingGame -> IO (Maybe OngoingGame)) -> MVar OngoingGame -> IO ()
handleGameSafely handler gvar = mask $ \restore -> do
    og <- takeMVar gvar
    r <- restore (handler og >>= evaluate) `onException` putMVar gvar og
    case r of
        (Just og') -> putMVar gvar og'
        Nothing -> putMVar gvar og

handleMove :: Connection -> PlayerNum -> Move -> OngoingGame -> IO (Maybe OngoingGame)
handleMove conn pn mv og = do
    {-TODO: consider finding a way to make observers joining independent of the players MVar
    mog <- tryTakeMVar mv
    case mog of
        Nothing -> err "other move being processed"
        -}
    let err msg = sendMsg (Err msg) conn >> return Nothing
    let tellAndPut act og' = do
            sendActionMsg act og'
            return (Just og')
    case tinfo og of
      Turn pn' _ _ | pn==pn' -> let gs' = applyEvent (Move mv) og
                                    history' = show mv : history og
                                    og' = og{gs=gs', history=history'} in case result gs' of
        Illegal -> tellAndPut (\ _ _ -> Attempt mv) og
        Win -> tellAndPut (NewState mv) og'{tinfo=Rule(otherNum pn)}
        Draw -> tellAndPut (NewState mv) og'{tinfo=Rule pn}
        Continue -> tellAndPut (NewState mv) og'{tinfo = Turn (otherNum pn) 0 0}
                  | otherwise -> err "Not your turn"
      Rule _ -> err "Game is not in progress"

handleRule :: Connection -> PlayerNum -> String -> OngoingGame -> IO (Maybe OngoingGame)
handleRule conn pn codeString og = do
    let err msg = sendMsg (Err msg) conn >> return Nothing
    case tinfo og of
      Turn _ _ _ -> err "Game is in progress"
      Rule pn' | pn/=pn' -> err "Not your turn to make a rule"
               | otherwise {-pn==pn'-} -> do
                    let qimps = zip [ "Prelude"
                                    , "Game.Chess.Fairy.Datatypes"
                                    , "Game.Chess.Fairy.Lib"
                                    , "Game.Chess.Fairy.Samples"]
                                    (repeat Nothing)
                          ++ map (second Just) [
                            ("Data.Map", "Map"),
                            ("Control.Arrow","Arrow"),
                            ("Control.Monad","Monad"),
                            ("Control.Applicative","Applicative")
                            ]
                    f <- liftIO$ runInterpreter$ (do
                           setImportsQ qimps
                           interpret codeString (as::Rule))
                    case f of
                        Left errmsg -> err (pack (fromErr errmsg))
                        Right r -> do
                            let og' = og{rules=(r:rules og), tinfo=Turn (otherNum pn) 0 0 }
                            let og'' = og'{gs=applyEvent Start og'}
                            sendInitMsg og''
                            return (Just og'')

applyEvent :: Event -> OngoingGame -> GameState
applyEvent e og = foldr ($) (innerRules center) (outerRules: rules og) e (gs og)


-- Message sending functions

sendInitMsg :: OngoingGame -> IO ()
sendInitMsg = sendActionMsg Init

sendActionMsg :: (ExpectedAction -> GameState -> ServerMsg) -> OngoingGame -> IO ()
sendActionMsg action og@OG{tinfo=(Turn pn _ _),gs} = do
    sequence_ (sendMsg (action YourMove gs) <$>  maybe (Leaf Nothing) snd (getPlayers pn og))
    sequence_ (sendMsg (action Wait gs) <$>  maybe (Leaf Nothing) snd (getPlayers (otherNum pn) og))
    sequence_ (sendMsg (action Observe gs) <$>  observers og)
sendActionMsg action og@OG{tinfo=(Rule pn),gs} = do
    sequence_ (sendMsg (action YourRule gs) <$>  maybe (Leaf Nothing) snd (getPlayers pn og))
    sequence_ (sendMsg (action WaitRule gs) <$>  maybe (Leaf Nothing) snd (getPlayers (otherNum pn) og))
    sequence_ (sendMsg (action Observe gs) <$>  observers og)

sendWelcomeMsg :: OngoingGame -> PlayerNum -> Connection -> IO ()
sendWelcomeMsg OG{tinfo=(Turn pn _ _),gs} pn' conn =
    sendMsg (Init (if pn==pn' then YourMove else Wait) gs) conn
sendWelcomeMsg OG{tinfo=(Rule pn),gs} pn' conn =
    sendMsg (Init (if pn==pn' then YourRule else WaitRule) gs) conn

addObserver :: Connection -> OngoingGame -> (OngoingGame, Int)
addObserver conn og@OG{observers=os} = let (t',n) = add conn os in (og{observers=t'} , n)

removeOnClose :: Connection -> Int -> MVar OngoingGame -> IO ()
removeOnClose conn n gvar = forever (receive conn) `finally` do
    og@OG{observers=os} <- takeMVar gvar
    let os' = remove n os
    putMVar gvar og{observers=os'}

sendMsg :: ServerMsg -> Connection -> IO ()
sendMsg (Err msg) conn = sendTextData conn (cons 'e' msg)
sendMsg (Attempt mv) conn = sendTextData conn (pack ('a':moveToString mv))
sendMsg (NewState mv exp gs) conn = sendTextData conn (pack ('s':toChar exp : moveToString mv ++ drawBoard (board gs)))
sendMsg (Init exp gs) conn = sendTextData conn (pack ('i':toChar exp : drawBoard (board gs)))

toChar :: ExpectedAction -> Char
toChar YourMove = 'm'
toChar YourRule = 'y'
toChar Wait = 'w'
toChar WaitRule = 'u'
toChar Observe = 'o'

moveToString :: Move -> String
moveToString ((a,b),(c,d)) = [chr | x<- [a,b,c,d], chr <- show x]

close :: Connection -> IO ()
close conn = do
    sendClose conn (""::Text)
    forever (receive conn)

parseMsg :: String -> Maybe ClientMsg
parseMsg "random" = Just RandomMove -- acceptable because "andom" is not a valid rule
parseMsg ('r':code) = Just$ NewRule code
parseMsg dat =
    let mpos = mapM (readMaybe.(:[])) dat in
    case mpos of
        Just [sx,sy,dx,dy] -> Just (MakeMove ((sx,sy),(dx,dy)))
        _ -> Nothing


fromErr :: InterpreterError -> String
fromErr (WontCompile errs) = Prelude.unlines (map frghc errs)
fromErr e = "weird error: "++ show e
frghc :: GhcError -> String
frghc GhcError{errMsg=m} = m


-- |Data structure for a set to which things may be added(at an unspecified index) or deleted(given the index returned when they were added)
-- operations have time complexity O(log(n)) where n is the maximum number of items the structure has contained simultaneously
data Tree a = Branch Int Int (Tree a) (Tree a) | Leaf (Maybe a) deriving (Foldable,Show,Functor)
--              depth, capacity, left, right

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
