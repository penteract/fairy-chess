{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Game.Chess.Fairy.Utils where
import GHC.IO (unsafePerformIO)
import System.Random
import Data.List (unfoldr)
import Control.Arrow(first)

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


up :: Show b => b -> a -> a
up a b = seq (unsafePerformIO (print a)) b

data Tree a = Node [a] | Branch Int (Tree a) (Tree a)

pickAt :: Tree a -> Int -> (a,Maybe (Tree a))
pickAt (Node xs) m = case splitAt m xs of
  ([],[x]) -> (x, Nothing)
  ([],x:xs) -> (x, Just$ Node xs)
  (xs,[x]) -> (x, Just$ Node xs)
  (xs,x:xs') -> (x, Just$ Branch m (Node xs) (Node xs'))
pickAt (Branch n l r) m  = if m<n then case pickAt l m of
  (x,Nothing) -> (x,Just$ r)
  (x,Just t) -> (x, Just$ Branch (n-1) t r)
  else case pickAt r (m-n) of
  (x,Nothing) -> (x,Just$ l)
  (x,Just t) -> (x, Just$ Branch n l t)



lazyShuffle :: RandomGen g => Int -> [a] -> g -> ([a],g)
lazyShuffle 0 xs g = ([],g)
lazyShuffle len xs g = let
  (g',g'')= split g -- split it, so that we can use it lazily
  permute m ((a,b),c) = (a,fmap (,c,m) b)
  in (unfoldr ((\(t,g,m) -> permute (m-1) (first (pickAt t . (`mod`m)) (random g)) )<$>) (Just (Node xs, g', len)) , g'')