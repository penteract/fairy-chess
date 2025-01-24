{-#LANGUAGE NamedFieldPuns #-}
module Game.Chess.Fairy.Lib where
import Game.Chess.Fairy.Datatypes
import Control.Applicative(liftA2)
import Data.List (nub)
import Control.Arrow(second)
import Game.Chess.Fairy.Utils (up)

move :: Move -> Board -> Board
move (src,dst) board = set dst (board!@src) $ set src Empty board

applyMove :: Move -> GameState -> GameState
applyMove m gs@GS{board} = gs{board=move m board, pieceMoved=True}

rIllegal :: Rule
rIllegal _ _ gs = gs{result=Illegal}

doBefore :: Step -> Rule
doBefore f inner e gs = inner e (f gs)

doAfter :: Step -> Rule
doAfter f inner e gs = f (inner e gs)

isStart :: GEGSto Bool
isStart inner Start gs = True
isStart inner e gs = False

isMove = not_ isStart

-- | Returns True if the value of the named variable is not 0.
boolVar :: String -> GEGSto Bool
boolVar v act e gs = readVar v gs /= 0

type GEGSto a = Game -> Event -> GameState -> a

-- | Combines 2 tests. Returns True if both do.
(~&~) :: GEGSto Bool -> GEGSto Bool -> GEGSto Bool
(~&~) = liftA2(liftA2(liftA2(&&)))

-- | Negates a test.
not_ :: GEGSto Bool -> GEGSto Bool
not_ = fmap$fmap$fmap not

__ f = \ _ _ -> f

--  Making this behave as the monadic version would require
-- | Given a condition, implement a rule only when that condition holds.
when :: GEGSto Bool -> Rule -> Rule
when f r act e gs = if f act e gs then r act e gs else act e gs
-- when f r = whether f r id


-- This could be given the more general type @(GEGSto Bool) -> GEGSto a -> GEGSto a -> GEGSto a@
-- | Given a condition, do one thing when the condition holds and another when it doesn't.
whether :: GEGSto Bool -- ^ @if@
           -> Rule -- ^ @then@
           -> Rule -- ^ @else@
           -> Rule
whether f r1 r2 act e gs = if f act e gs then r1 act e gs else r2 act e gs
-- whether = liftA2$liftA2$liftA2$ if'

-- |Helps to access a result. Like monadic bind.
with :: (GEGSto a) -> (a->Rule) -> Rule
with getter body inner e gs = body (getter inner e gs) inner e gs

withMove :: (Move -> Rule) -> Rule
withMove f inner Start gs = inner Start gs
withMove f inner e@(Move m) gs = f m inner e gs

withPeekResult :: ((Event -> GameState -> Result) -> Rule) -> Rule
withPeekResult f inner e gs@GS{peekResult=Just peek} = f peek inner e gs
withPeekResult f inner e gs@GS{peekResult=Nothing} = inner e gs

-- | Upon a condition being satisfied, implement a rule until a second condition is satisfied.
uponDoUntil :: String -> GEGSto Bool -> Rule -> GEGSto Bool -> Rule
uponDoUntil v upon r untl =  when upon (doAfter$ setVar v 1)
                 . when untl (doAfter$ setVar v 0)
                 . when (boolVar v) r


srcIsPlayerPiece :: Piece -> GEGSto Bool
srcIsPlayerPiece p _ (Move (src,dst)) GS{turn,board} = board!@src == Occupied turn p
srcIsPlayerPiece _ _ _ _ = False

type MoveDesc = Move -> GameState -> Bool

addMovesAnd :: Piece -> MoveDesc -> Step -> Rule -- Add moves and do something when a move actually happens
addMovesAnd p desc extra = when (__ (not.pieceMoved) ~&~ srcIsPlayerPiece p) $
    withMove (\ m -> when (__$ desc m) (doBefore (extra . applyMove m)))

addMoves :: Piece -> MoveDesc -> Rule
addMoves p desc = addMovesAnd p desc id

setMoves :: Piece -> MoveDesc -> Rule
setMoves p desc = addMoves p desc . preventMovesBy p

preventMovesBy :: Piece -> Rule
preventMovesBy p = when (__ (not.pieceMoved) ~&~ srcIsPlayerPiece p) rIllegal

rider :: [(Int,Int)] -> MoveDesc
rider ms' (src,dst) gs = let
    ms = case turn gs of White -> ms'; Black -> map (second negate) ms'
    delta = toInts dst - toInts src
    mx (a,b) = if abs a>abs b then a else b
    in color (getAt dst gs) /= Just (turn gs) &&  any (\ m -> let numSteps = mx delta `div` mx m in
        numSteps>0 && fromIntegral numSteps * m == delta && clearPath (map ((toInts src+).(m*).fromIntegral) [1 .. numSteps - 1]) gs
        ) ms

clearPath :: [(Int, Int)] -> GameState -> Bool
clearPath ps gs = all (\ p -> Just Empty == getAtInts p gs) ps

leaper :: [(Int,Int)] -> MoveDesc
leaper ms' (src,dst) gs = let
    ms = case turn gs of White -> ms'; Black -> map (second negate) ms'
    delta = toInts dst - toInts src
    in color (getAt dst gs) /= Just (turn gs) && delta `elem` ms

reflections :: [(Int,Int)] -> [(Int,Int)]
reflections = nub . (>>=  \(x,y)-> [(x,y),(y,x)]>>= \(x,y)-> [(x,y),(-x,y)]>>= \(x,y)->[(x,y),(x,-y)] )

addCondition :: MoveDesc -> MoveDesc -> MoveDesc
addCondition cond desc mv gs = cond mv gs && desc mv gs

(^^&&^^) :: Applicative a => Applicative a2 => a (a2 Bool) -> a (a2 Bool) -> a (a2 Bool)
(^^&&^^) = liftA2 $ liftA2 (&&)
(^^||^^) :: Applicative a => Applicative a2 => a (a2 Bool) -> a (a2 Bool) -> a (a2 Bool)
(^^||^^) = liftA2 $ liftA2 (||)

capturing :: MoveDesc
capturing (src,dst) gs = getAt dst gs /= Empty

nonCapturing :: MoveDesc
nonCapturing (src,dst) gs = getAt dst gs == Empty

unmoved :: MoveDesc
unmoved (src,dst) gs = readVar ("moved"++show src) gs == 0

trackMoved :: Rule -> Rule -- If a rule causes a piece to move, record that at the destination
trackMoved r = whether (__ pieceMoved) r (r . when (__ pieceMoved) (withMove (\ (src,dst) -> doBefore (setVar ("moved"++show dst) 1)))) . clearMoved

clearMoved :: Rule
clearMoved = when isStart (doBefore $ foldr (.) id [ setVar ("moved"++show pos) 0 | pos <- allPos])

allPos :: [BoardPos]
allPos = [(file, rank) | file<-[A ..], rank<-[A ..]]
allMoves :: [Move]
allMoves = [(p1,p2) | p1 <- allPos, p2 <- allPos]

setInState :: BoardPos -> Square -> GameState -> GameState
setInState pos sq gs@GS{board} = gs{board=set pos sq board}

changeTurn :: Step
changeTurn gs = gs{turn=other (turn gs)}
