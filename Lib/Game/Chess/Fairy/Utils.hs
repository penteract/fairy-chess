{-# LANGUAGE FlexibleInstances #-}
module Game.Chess.Fairy.Utils where

instance Num (Int,Int) where
  (a,b) + (w,x) = (a+w,b+x)
  (a,b) * (w,x) = (a*w,b*x)
  negate v = (-1,-1) * v
  abs v = v
  signum (0,0) = 0 -- highest common factor would also satisfy the laws here and could be useful
  signum _ = 1
  fromInteger n = let m = fromInteger n in (m,m)

safeToEnum :: (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i = let r = toEnum i
                   max = maxBound `asTypeOf` r
                   min = minBound `asTypeOf` r
               in if i >= fromEnum min && i <= fromEnum max
               then Just r
               else Nothing
