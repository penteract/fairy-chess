{-
rule ideas:
  can't make the aame opening move as in the previous game
  ultima ruleset
-}
module Game.Chess.Fairy.Samples where
import Game.Chess.Fairy.Lib
import Game.Chess.Fairy.Datatypes

moveToInt :: Move -> Int
moveToInt ((a,b),(c,d)) = 1 + (foldl (\ x y -> x*8+fromEnum y) 0 [a,b,c,d])

intToMove :: Int -> Move
intToMove n = ((toEnum a,toEnum b),(toEnum c,toEnum d))
  where
    (n',d) = (n-1) `divMod` 8
    (n'',c) = n' `divMod` 8
    (a,b) = n'' `divMod` 8

___ = const.const.const
-- Allow precommiting to a move which happens on the next turn
rPrecommit :: Rule
rPrecommit inner = with (__ turn) (\p ->
  let vName = "precommit" ++ show p in
    with (__$ readVar vName) (\ preMoveN -> -- apply any precommitted move if legal
      when  (___ (preMoveN/=0))
        (let preMove = intToMove preMoveN in
          with (__$ inner (Move preMove)) (\ gs' -> -- we need to use 'inner', rather than the game being passed here because we don't want the precommitted move to be another precommitment
            when (___ (result gs'/=Illegal))
              (\ act e gs -> act e gs'{turn=p} )
          )
        )
    ) . (doBefore (setVar vName 0)) . -- clear precommitted moves whether or not they worked
    -- Now handle moves which could be precommitments(entered backwards)
    withMove (\ (src,dst) ->
      when (\ act e gs -> not (pieceMoved gs) && result (act (Move (dst,src)) gs) /= Illegal) (
        \ act e gs -> setVar vName (moveToInt (dst,src)) (changeTurn gs)
      )
    )
  )inner
           -- don't use peekResult because it won't work sensibly

--            if (isReasonable mv gs) doBefore (inner mv) else doNothing) .

--  withMove (\ m -> )
