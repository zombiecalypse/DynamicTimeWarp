{-# LANGUAGE TemplateHaskell #-}
import Data.Vector.DynamicTimeWarping
import Data.Vector.Unboxed (fromList)
import Test.QuickCheck

dist l r = distanceBy (\x y -> (x-y)^2) l r

prop_Reflexivity :: [Float] -> [Float] -> Bool
prop_Reflexivity l r =
  let l' = fromList l
      r' = fromList r
  in abs (dist l' r' - dist r' l') <= 1e-10 ||
        (dist l' r' == 1/0 && dist r' l' == 1/0)

prop_NonNegative :: [Float] -> [Float] -> Bool
prop_NonNegative l r =
  let l' = fromList l
      r' = fromList r
  in dist l' r' >= 0

main = $quickCheckAll
