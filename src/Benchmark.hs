import Control.Monad
import Control.Arrow (first, second)
import Control.Applicative
import Data.Vector.DynamicTimeWarping
import Data.Vector.Unboxed (fromList, Vector, postscanl)
import System.Random
import Text.Printf

import Criterion
import Criterion.Main

genSeq :: Int -> IO (Int, Vector (Float, Float))
genSeq n = do
  let genTuple = (,) <$> randomRIO (0,100) <*> randomRIO (0,100)
  (,) n <$> postscanl (\(x,y) (z,w) -> (x+z,y+w)) (0,0) . fromList <$> replicateM n genTuple

genSeqs (l, r) = do
  left <- genSeq l
  right <- genSeq r
  return (left, right)

setupEnv = do
  let sizes = [(l, r) | l <- [5,10,100, 1000, 10000], r <- [5,10,100, 1000, 10000], l <= r] :: [(Int, Int)]
  mapM genSeqs sizes

dist (l, r) = distanceBy (\(x1,x2) (y1,y2) -> (x1-y1)^2 + (x2-y2)^2) l r

main = defaultMain [
  env setupEnv $ \ ~l ->
    bgroup "dtw" [
      bench (printf "dtw(%03d, %03d)" l r) $ nf dist (ll,rr) | ((l, ll), (r, rr)) <- l
    ]
  ]
