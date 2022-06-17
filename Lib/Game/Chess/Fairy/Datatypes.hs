{-# LANGUAGE NamedFieldPuns #-}
module Game.Chess.Fairy.Datatypes where
import Game.Chess.Fairy.Utils

import System.Random
import qualified Data.Map as Map
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import Control.Applicative (liftA2)
import Data.List (intercalate)

data Piece = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Enum, Show)
data Player = Black | White deriving (Eq, Show)
data Square = Empty | Occupied Player Piece deriving (Eq, Show)

color :: Square -> Maybe Player
color Empty = Nothing
color (Occupied pl _) = Just pl

other :: Player->Player
other Black = White
other White = Black

type T8 a = (a,a,a,a,a,a,a,a)
eight :: a -> T8 a
eight x = (x,x,x,x,x,x,x,x)

t8map :: (t -> h) -> (t, t, t, t, t, t, t, t) -> (h, h, h, h, h, h, h, h)
t8map fn (a,b,c,d,e,f,g,h) = (fn a,fn b,fn c,fn d,fn e,fn f,fn g,fn h) 

toList :: T8 a -> [a]
toList (a,b,c,d,e,f,g,h) = [a,b,c,d,e,f,g,h]

data Index = A | B | C | D | E | F | G | H deriving (Bounded,Eq,Enum,Show,Read)

index :: T8 a -> Index -> a
index (a,b,c,d,e,f,g,h) A = a
index (a,b,c,d,e,f,g,h) B = b
index (a,b,c,d,e,f,g,h) C = c
index (a,b,c,d,e,f,g,h) D = d
index (a,b,c,d,e,f,g,h) E = e
index (a,b,c,d,e,f,g,h) F = f
index (a,b,c,d,e,f,g,h) G = g
index (a,b,c,d,e,f,g,h) H = h

setAt :: Index -> a -> T8 a -> T8 a
setAt A a (_,b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
setAt B b (a,_,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
setAt C c (a,b,_,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
setAt D d (a,b,c,_,e,f,g,h) = (a,b,c,d,e,f,g,h)
setAt E e (a,b,c,d,_,f,g,h) = (a,b,c,d,e,f,g,h)
setAt F f (a,b,c,d,e,_,g,h) = (a,b,c,d,e,f,g,h)
setAt G g (a,b,c,d,e,f,_,h) = (a,b,c,d,e,f,g,h)
setAt H h (a,b,c,d,e,f,g,_) = (a,b,c,d,e,f,g,h)

transpose :: T8 (T8 a) -> T8 (T8 a)
transpose ((a1,a2,a3,a4,a5,a6,a7,a8)
          ,(b1,b2,b3,b4,b5,b6,b7,b8)
          ,(c1,c2,c3,c4,c5,c6,c7,c8)
          ,(d1,d2,d3,d4,d5,d6,d7,d8)
          ,(e1,e2,e3,e4,e5,e6,e7,e8)
          ,(f1,f2,f3,f4,f5,f6,f7,f8)
          ,(g1,g2,g3,g4,g5,g6,g7,g8)
          ,(h1,h2,h3,h4,h5,h6,h7,h8)) = ((a1,b1,c1,d1,e1,f1,g1,h1)
                                       ,(a2,b2,c2,d2,e2,f2,g2,h2)
                                       ,(a3,b3,c3,d3,e3,f3,g3,h3)
                                       ,(a4,b4,c4,d4,e4,f4,g4,h4)
                                       ,(a5,b5,c5,d5,e5,f5,g5,h5)
                                       ,(a6,b6,c6,d6,e6,f6,g6,h6)
                                       ,(a7,b7,c7,d7,e7,f7,g7,h7)
                                       ,(a8,b8,c8,d8,e8,f8,g8,h8))

revt8 :: T8 a -> T8 a
revt8 (a,b,c,d,e,f,g,h) = (h,g,f,e,d,c,b,a)

type File = T8 Square
type Board = T8 File

type BoardPos = (Index,Index)

(!@) :: Board -> BoardPos -> Square
board !@ (file,rank) = (board `index` file) `index` rank

set :: BoardPos -> Square -> Board -> Board
set (file,rank) sq board =
    let newFile = setAt rank sq (board `index` file)
    in setAt file newFile board

type Move = (BoardPos,BoardPos)

middle :: Move -> BoardPos
middle ((sf,sr),(df,dr)) = (toEnum ((fromEnum sf + fromEnum df) `div` 2), toEnum ((fromEnum sr + fromEnum dr) `div` 2))

data Event = Start | Move Move deriving (Read)
data Result = Continue | Win | Draw | Illegal deriving (Eq,Show)

data GameState = GS {
    turn :: Player -- Who's turn is it (changes at the center)
  , board :: Board -- What pieces are where. This is the only part of the gamestate shown to players
  , randg :: StdGen -- Random numbers
  , varMap :: Map.Map String Int -- Any extra data that needs storing
  , pieceMoved :: Bool -- If true on the way in, it indicates that an outer rule has applied a move and inner rules should not do so themselves (although they may mark the result as illegal).
  -- Set to False at the center and should be left alone on the way out.
  , peekResult :: Maybe (Event -> GameState -> Result) -- Reruns the whole rule stack the given move and gives the result. Not availalbe when detecting check
     -- Necessary to prevent castling through/out of check, should be called very rarely for performance reasons
  , result :: Result -- result of applying the move. Continue by default, should be set to illegal for illegal moves, and is set to `Win` by an outer rule when a king is captured
  -- The outermost rule makes moving into check illegal and putting your opponent in a position where they have no legal moves a win or a draw
}

emptyState :: GameState
emptyState = GS White (eight (eight Empty)) undefined Map.empty False Nothing Continue

type Step = GameState -> GameState
type Game = Event -> Step
type Rule = Game -> Game

getAt :: BoardPos -> GameState -> Square
getAt pos = (!@ pos) . board

modifyBoard :: (Board -> Board) -> Step
modifyBoard f gs@GS{board} = gs{board=f board}

readVar :: String -> GameState -> Int
readVar s gs = Map.findWithDefault 0 s (varMap gs)

setVar :: String -> Int -> Step
setVar s i gs = gs{varMap = Map.insert s i (varMap gs)}

modifyVar :: String -> (Int -> Int) -> Step
modifyVar s f gs = setVar s (f $ readVar s gs) gs

toInts :: BoardPos -> (Int,Int)
toInts (file,rank) = (fromEnum file, fromEnum rank) 
fromInts :: (Int,Int) -> Maybe BoardPos
fromInts (file,rank) = liftA2 (,) (safeToEnum file) (safeToEnum rank)

getAtInts :: (Int,Int) -> GameState -> Maybe Square
getAtInts mv gs = fmap (flip getAt gs) (fromInts mv)

toInt' :: BoardPos -> Int
toInt' (file,rank) = 8*(fromEnum file) + fromEnum rank

toInt :: Move -> Int
toInt (src,dst) = 64*(toInt' src) + toInt' dst

drawBoard :: Board -> String
drawBoard = 
  intercalate "\n" . reverse . toList . t8map (toList . (t8map drawSquare)) . transpose

whiteps = ['♙','♖','♘','♗','♕','♔']
blackps = ['♟','♜','♞','♝','♛','♚']
drawSquare :: Square -> Char
drawSquare Empty = ' '
drawSquare (Occupied pl p) = case pl of
  White -> whiteps !! fromEnum p
  Black -> blackps !! fromEnum p

showState :: GameState -> String
showState (GS t b rnd v pm peek res) = intercalate "\n" [show t, drawBoard b, show v, "pieceMoved"++show pm, show res]