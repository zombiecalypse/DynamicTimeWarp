module Data.Vector.DynamicTimeWarping where
import Control.Monad (forM_)
import qualified Data.Vector.Generic as V
import qualified Data.Array.ST as ST
import qualified Data.Array.MArray as MArray
import qualified Data.Array.Base as Arr

distanceBy :: V.Vector v a => (a -> a -> Float) -> v a -> v a -> Float
distanceBy d left right =
  let lenl = V.length left
      lenr = V.length right
      def = 1/0
      tabl = ST.runSTUArray $ do
        table <- MArray.newArray_ ((0,0), (lenl, lenr))
        let write = MArray.writeArray table
            read_ = MArray.readArray table
        write (0,0) 0
        forM_ [1..lenl] $ \i -> write (i, 0) def
        forM_ [1..lenr] $ \j -> write (0, j) def
        forM_ [1..lenl] $ \i ->
          forM_ [1..lenr] $ \j -> do
            ins <- read_ (i-1,j)
            del <- read_ (i,j-1)
            mat <- read_ (i-1,j-1)
            let cost = d (left V.! (i-1)) (right V.! (j-1))
                rec = ins `min` del `min` mat
            write (i,j) $ cost + rec
        return table
  in tabl Arr.! (lenl, lenr)
