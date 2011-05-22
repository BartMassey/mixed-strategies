-- Copyright © 2011 Bart Massey

-- Calculate an optimal mixed strategy given
-- the payoff matrix of a two-player zero-sum
-- single-round iterated simultaneous game.
-- Follows the method of Chapter 6 of
--   J.D. Williams. The Compleat Strategist. McGraw-Hill 1954. 

import Data.Array
import Data.Function (on)
import Data.List
import Data.Ord (comparing)
import Text.Printf

data Schema = Schema {
  offset :: Double,
  payoffs :: Array (Int, Int) Double,
  namer :: Array Int Int,
  namec :: Array Int Int,
  d :: Double
}

instance Show Schema where
  show s =
    printf "offset = %g, d = %g\n" (offset s) (d s) ++
      "  " ++ printVec " %5d" (namec s) ++ "\n" ++
      concatMap printRow [1 .. nr - 1] ++
      "  " ++ printRowM nr
    where
      ((1, 1), (nr, nc)) = bounds $ payoffs s
      printVec fmt a =
        let (1, n) = bounds a in
        concatMap (printf fmt . (a !)) [1..n]
      printRowM i =
        printVec " %5.2f" (ixmap (1, nc) (\j -> (i, j)) (payoffs s))
      printRow i =
        printf "%2d" (namer s ! i) ++ printRowM i ++ "\n"

readSchema :: IO Schema
readSchema =
  construct `fmap` getContents
  where
    construct s = 
      let ps = checkPayoffs $ map (map read . words) $ lines s in
      let ((1, 1), (nr, nc)) = bounds ps in
      let o = minimum $ elems ps in
      let core = map (\(c, x) -> (c, x - o)) $ assocs ps
          augr = zip (zip (repeat (nr + 1)) [1..nc]) (repeat (-1))
          augc = zip (zip [1..nr] (repeat (nc + 1))) (repeat 1)
          augv = ((nr + 1, nc + 1), 0)
          bounds' = ((1, 1), (nr + 1, nc + 1)) in
      let ps' = array bounds' $ core ++ augr ++ augc ++ [augv] in
      Schema {
        offset = o,
        payoffs = ps',
        namer = listArray (1, nr) $ take nr [1..],
        namec = listArray (1, nc) $ take nc [1..],
        d = 1 }
      where
        checkPayoffs :: [[Double]] -> Array (Int, Int) Double
        checkPayoffs l@(e : es)
          | all ((== length e) . length) es =
              listArray ((1, 1), (length l, length e)) $ concat l
        checkPayoffs _ = error "bad payoff matrix format"

pivot :: Schema -> Schema
pivot s =
  Schema {
    offset = offset s,
    payoffs = updatePayoffs,
    namer = namer s,
    namec = namec s,
    d = pv }
  where
    ps = payoffs s
    ((1, 1), (nr, nc)) = bounds ps
    ((pr, pc), pv) = 
        maximumBy pivotCompare $
        map (minimumBy pivotCompare) $
        groupBy ((==) `on` (snd . fst)) potPivots
        where
          potPivots = 
            filter okPivot $ assocs ps
            where
              okPivot ((r, c), v) = 
                r < nr && c < nc && v > 0 && (ps ! (nr, c)) < 0
          pivotCompare =
            comparing pivotCriteria
            where
              pivotCriteria ((r, c), p) =
                - (ps ! (nr, c)) * (ps ! (r, nc)) / p
    updatePayoffs = 
      listArray (bounds ps) $ map updatePayoff $ assocs $ ps
      where
        updatePayoff ((r, c), n)
          | r == pr && c == pc = d s
          | r == pr = n
          | c == pc = -n
          | otherwise = (n * pv - (ps ! (pr, c)) * (ps ! (r, pc))) / d s

main :: IO ()
main = do
  s0 <- readSchema
  print s0
