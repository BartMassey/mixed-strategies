-- Copyright © 2011 Bart Massey

-- Calculate an optimal mixed strategy given
-- the payoff matrix of a two-player zero-sum
-- single-round iterated simultaneous game.
-- Follows the method of Chapter 6 of
--   J.D. Williams. The Compleat Strategist. McGraw-Hill 1954. 

import Data.Array
import Text.Printf

data Schema = Schema {
  offset :: Double,
  payoffs :: Array (Int, Int) Double,
  augr :: Array Int Double, 
  augc :: Array Int Double,
  namer :: Array Int Int,
  namec :: Array Int Int,
  v :: Double,
  d :: Double
}

instance Show Schema where
  show s =
    printf "offset = %g, d = %g\n" (offset s) (d s) ++
      "  " ++ printVec " %5d" (namec s) ++ "\n" ++
      concatMap printRow [1..nr] ++
      "  " ++ printVec " %5.2f" (augc s) ++ printf " %5.2f\n" (v s)
    where
      ((1, 1), (nr, nc)) = bounds $ payoffs s
      printVec fmt a =
        let (1, n) = bounds a in
        concatMap (printf fmt . (a !)) [1..n]
      printRow i =
        printf "%2d" (namer s ! i) ++
        printVec " %5.2f" (ixmap (1, nc) (\j -> (i, j)) (payoffs s)) ++
        printf " %5.2f\n" (augr s ! i)

readSchema :: IO Schema
readSchema =
  construct `fmap` getContents
  where
    construct s = 
      let ps = checkPayoffs $ map (map read . words) $ lines s in
      let ((1, 1), (nr, nc)) = bounds ps in
      let o = minimum $ elems ps in
      let ps' = listArray ((1, 1), (nr, nc)) $ 
                map (\x -> x - o) $ elems ps in
      Schema {
        offset = o,
        payoffs = ps',
        augr = listArray (1, nr) $ replicate nr 1,
        augc = listArray (1, nc) $ replicate nc (-1),
        namer = listArray (1, nr) $ take nr [1..],
        namec = listArray (1, nc) $ take nc [1..],
        v = 0,
        d = 1 }
      where
        checkPayoffs :: [[Double]] -> Array (Int, Int) Double
        checkPayoffs l@(e : es)
          | all ((== length e) . length) es =
              listArray ((1, 1), (length l, length e)) $ concat l
        checkPayoffs _ = error "bad payoff matrix format"

main :: IO ()
main = do
  s0 <- readSchema
  print s0
