{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Game.Chess.Fairy.BaseGame where

import Game.Chess.Fairy.Datatypes
import Game.Chess.Fairy.Lib
import Control.Arrow (second)


initialLayout ::Board
initialLayout =
    let p8 = eight Pawn
        e8 = eight Empty
        backrank = (Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook)
        white = Occupied White
        black = Occupied Black in
    transpose
    (t8map white backrank
    ,t8map white p8
    ,e8,e8,e8,e8
    ,t8map black p8
    ,t8map black backrank)

doNothing :: Game
doNothing event gs = gs

changeTurn :: Step
changeTurn gs = gs{turn=other (turn gs),pieceMoved=False}
rNoMoveIllegal :: Rule -- If no outer rule has set `pieceMoved`, the attempted move is illegal
rNoMoveIllegal = when (isMove ~&~ (\ _ _ -> not.pieceMoved)) rIllegal
rSetLayout :: Rule
rSetLayout = when isStart (doAfter (\gs -> gs{board=initialLayout}))
rBishopMoves = setMoves Bishop (rider (reflections [(1,1)]))
rRookMoves = trackMoved $ setMoves Rook (rider (reflections [(0,1)]))
rQueenMoves = setMoves Queen (rider (reflections [(0,1),(1,1)]))
rKnightMoves = setMoves Knight (leaper (reflections [(1,2)])) -- reflections in diagonals are counted


rPawnMoves :: Rule
rPawnMoves = trackMoved $
    addMoves Pawn (capturing ^^&&^^ leaper [(-1,1),(1,1)]) -- automatically reflected for black
  . addMovesAnd Pawn (leaper [(-1,1),(1,1)] ^^&&^^ (\ (_,dst) gs -> readVar "pawnSkipped" gs == toInt' dst) )
      (\ gs -> modifyBoard (maybe id (flip set Empty) (fromInts (readVar "skippingPawn" gs `divMod` 8))) gs)
  . doBefore (setVar "pawnSkipped" (-1) . setVar "skippingPawn" (-1)) -- This also applies at the start
  . withMove (\m -> addMovesAnd Pawn (unmoved ^^&&^^ nonCapturing ^^&&^^ leaper [(0,2)])
                      (setVar "pawnSkipped" (toInt' (middle m)) . setVar "skippingPawn" (toInt' (snd m)))) -- track double moves
  . setMoves Pawn (nonCapturing ^^&&^^ leaper [(0,1)])

castling :: Rule
castling = withMove (\ (src,dst) -> withPeekResult (\ peek -> with (__ id) (\ gs -> let 
  delta = toInts dst - toInts src
  notCheck pos = all ((Win/=) . flip peek (applyMove (src,pos) gs).Move) allMoves
  unmovedRook pos = getAt pos gs == Occupied (turn gs) Rook && unmoved (pos,pos) gs
  castle :: BoardPos -> [Index] -> Rule
  castle rookPos path = addMovesAnd King (unmoved ^^&&^^ (__ (unmovedRook rookPos)) -- king and rook are unmoved
                                         ^^&&^^ const (clearPath [toInts (file,snd src) | file <- drop 1 (init path)]) -- test whether squares between the king and rook are empty
                                         ^^&&^^ (__ (notCheck src && notCheck (middle (src,dst)))) ) -- king can't castle out of check or through check
                          (applyMove (rookPos, (middle (src,dst)))) -- Interesting behaviour if rook is too close to king
  in case delta of
    (2,0) -> castle (second (const H) src) ([H,G .. snd src])
    (-2,0) -> castle (second (const A) src) ([A,B .. snd src])
    _ -> id)))

rKingMoves :: Rule
rKingMoves = trackMoved $
    castling
  . setMoves King (leaper (reflections [(0,1),(1,1)]))

innerRules :: Rule
innerRules = rKingMoves . rPawnMoves . rKnightMoves . rQueenMoves . rRookMoves . rBishopMoves . rSetLayout. rNoMoveIllegal . doAfter changeTurn

outerRules :: Rule
outerRules = rCheckmate . rNoKingMeansWin

rCheckmate :: Rule -- If every move of the opponent allows you to win, then win. Applying this more than once is not recommened for performance reasons.
rCheckmate inner e gs =
    let allPos = [(file, rank) | file<-[A ..], rank<-[A ..]] -- TODO: shuffle this list
        allMoves = [(p1,p2) | p1 <- allPos, p2 <- allPos]
        noPeek :: Game
        noPeek e gs = inner e gs{peekResult=Nothing} -- Alternative: `inner e gs{peekResult=Just const const Illegal}`
        withPeek :: Game
        withPeek e gs = inner e gs{peekResult = Just (((.).(.)) result noPeek)}
        r = withPeek e gs
        in if result r /= Continue then r
        else let ply1 = filter ((Illegal /=).result.fst) [(withPeek (Move m) gs, m) | m <- allMoves] in
            if any (\(_,m) -> result (noPeek (Move m) gs) == Win) ply1 then r{result=Illegal}  -- noPeek is used to test the result, so that if we thought a move was legal, it is still considered legal
              else if any ((Draw==).result.fst) ply1 then r
                else case forcedWin noPeek ply1 allMoves of
                  Nothing -> r{result=Win} -- This covers the case where the opponent has no legal moves TODO: consider draws - allow a null move event?
                  (Just m) -> setVar "legalMove" (toInt m) r

forcedWin :: Game -> [(GameState,Move)] -> [Move] -> Maybe Move -- Is there any gamestate to which no responses (tested in the order given) are wins
forcedWin noPeek ((g,m):gs) mvs = case break (\ mv -> result (noPeek (Move mv) g) == Win) mvs of
    (mvs, []) -> Just m -- The opponent cannot immediately win after m
    (nonWinning, winning:rest) -> forcedWin noPeek gs (winning:nonWinning++rest) -- put the winning move to the front to help with future searches
forcedWin noPeek [] mvs = Nothing

rNoKingMeansWin :: Rule
rNoKingMeansWin inner e gs = let r = inner e gs in
    if Occupied (turn r) King `elem` (toList (board r) >>= toList) then r else r{result=Win}