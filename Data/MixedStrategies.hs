-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | Calculate an optimal mixed strategy given the payoff
-- matrix of a two-player zero-sum single-round iterated
-- simultaneous game.  Follows the method of Chapter 6 of
-- J.D. Williams' \"The Compleat Strategyst\" (McGraw-Hill
-- 1954). Throughout, the player playing the rows (strategies
-- on the left) is assumed to be the \"maximizer\" and the
-- player playing the columns (strategies on top) is assumed
-- to be the \"minimizer\".

module Data.MixedStrategies (
  Schema(..), 
  Edge(..),
  readSchema, 
  pivot, 
  Solution(..), 
  extractSolution, 
  solved,
  solve
) where

import Prelude hiding (Left, Right, lookup)
import Data.Array
import Data.Function (on)
import Data.List hiding (lookup)
import Data.Map (lookup, fromList)
import Data.Ord (comparing)
import Text.Printf
import Text.SimpleTabular

dim :: Array Int t -> Int
dim a = let (1, ix) = bounds a in ix

dims :: Array (Int, Int) t -> (Int, Int)
dims a = let ((1, 1), ixs) = bounds a in ixs

-- | An 'Edge' of a 'Schema' contains annotations of the Schema.
data Edge = Left | Top | Right | Bottom deriving (Ord, Eq, Ix)

-- | Schema describing a two-player hidden information game.
data Schema = Schema {
  -- | Score offset to make the game fair.
  offset :: Double,
  -- | \"Determinant\".
  d :: Double,
  -- | Strategy \"names\", given as labels along an edge of the
  -- payoff matrix.
  names :: Array Edge (Array Int (Maybe Int)),
  -- | Payoff matrix.
  payoffs :: Array (Int, Int) Double
}

-- | Show a 'Schema' in tabular format.
instance Show Schema where
  show s =
    let tab =       
          ["" : printNameVec Top] ++
          map printRow [1 .. nr - 1] ++
          ["" : printRowM nr] ++
          ["" : printNameVec Bottom]
        in
    printf "O = %.2f, D = %.2f\n" (offset s) (d s) ++
    tabular (replicate (nc + 1) 'r' ++ "l") tab
    where
      (nr, nc) = dims $ payoffs s
      printRowM i =
        printVec $ ixmap (1, nc) (\j -> (i, j)) (payoffs s)
        where
          printVec a =
            map (printf "%.2f" . (a !)) [1 .. dim a]
      printName e i =
        case names s ! e ! i of
          Just v -> printf "%d" v
          Nothing -> ""
      printNameVec e =
        map (printName e) [1 .. dim (names s ! e)]
      printRow i =
        [printName Left i] ++ 
        printRowM i ++ 
        [printName Right i]

-- | Read a 'Schema' (actually a payoff matrix) from standard input.
readSchema :: IO Schema
readSchema =
  construct `fmap` getContents
  where
    construct s = 
      let o = minimum $ elems ps in
      let core = map (\(c, x) -> (c, x - o)) $ assocs ps
          augr = zip (zip (repeat (nr + 1)) [1..nc]) (repeat (-1))
          augc = zip (zip [1..nr] (repeat (nc + 1))) (repeat 1)
          augv = ((nr + 1, nc + 1), 0)
          bounds' = ((1, 1), (nr + 1, nc + 1)) in
      let ps' = array bounds' $ core ++ augr ++ augc ++ [augv] in
      Schema {
        offset = o,
        d = 1,
        names = mkNames,
        payoffs = ps' }
      where
        ps = mkPayoffs $ map (map read . words) $ lines s
        (nr, nc) = dims ps
        mkNames =
          array (Left, Bottom) [namesLeft, namesTop, namesRight, namesBottom]
          where
            namesLeft = (Left, listArray (1, nr) [Just n | n <- [1..nr]])
            namesTop = (Top, listArray (1, nc) [Just n | n <- [1..nc]])
            namesRight = (Right, listArray (1, nr) (replicate nr Nothing))
            namesBottom = (Bottom, listArray (1, nc) (replicate nc Nothing))
        mkPayoffs :: [[Double]] -> Array (Int, Int) Double
        mkPayoffs l@(e : es)
          | all ((== length e) . length) es =
              listArray ((1, 1), (length l, length e)) $ concat l
        mkPayoffs _ = error "bad payoff matrix format"

-- | Execute one pivot step in the 'Schema' reduction.
pivot :: Schema -> Schema
pivot s =
  Schema {
    offset = offset s,
    d = pv,
    names = updateNames,
    payoffs = updatePayoffs }
  where
    ps = payoffs s
    (nr, nc) = dims ps
    ((pr, pc), pv) = 
        maximumBy pivotCompare $
        map (minimumBy pivotCompare) $
        groupBy ((==) `on` (snd . fst)) $ 
        sortBy (comparing (snd . fst)) potPivots
        where
          potPivots = 
            filter okPivot $ assocs ps
            where
              okPivot ((_, c), v) = v > 0 && (ps ! (nr, c)) < 0
          pivotCompare =
            comparing pivotCriteria
            where
              pivotCriteria ((r, c), p) =
                - (ps ! (nr, c)) * (ps ! (r, nc)) / p
    updateNames = 
      array (Left, Bottom) [
        updateName Left Bottom pr pc,
        updateName Bottom Left pc pr,
        updateName Right Top pr pc,
        updateName Top Right pc pr ]
      where
        updateName e e' i i' =
          (e, (names s ! e) // [(i, names s ! e' ! i')])
    updatePayoffs = 
      listArray (bounds ps) $ map updatePayoff $ assocs $ ps
      where
        updatePayoff ((r, c), n)
          | r == pr && c == pc = d s
          | r == pr = n
          | c == pc = -n
          | otherwise = (n * pv - (ps ! (pr, c)) * (ps ! (r, pc))) / d s

-- | A game solution, given as the value of the game and
-- an optimal mixed strategy for each player.
data Solution = Solution {
  value :: Double,
  leftStrategy, topStrategy :: [Double] }

instance Show Solution where
  show soln =
    printf "value = %.2f\n" (value soln) ++
    printf "leftmax = %s\n" (showStrategy leftStrategy) ++
    printf "topmin = %s" (showStrategy topStrategy)
    where
      showStrategy strat =
        unwords $ map (uncurry (printf "%d:%.2f")) $ 
        zip [(1::Int)..] $ strat soln

-- | Given a solved 'Schema', extract the 'Solution'
-- implied by that 'Schema'.
extractSolution :: Schema -> Solution
extractSolution s | solved s = Solution {
  value = offset s + d s / (ps ! ds),
  leftStrategy = strat Bottom $ ixmap (1, nc - 1) (\j -> (nr, j)) ps,
  topStrategy = strat Right $ ixmap (1, nr - 1) (\j -> (j, nc)) ps }
  where
    ps = payoffs s
    ds@(nr, nc) = dims ps
    strat e odds =
      let strats = zip (elems (names s ! e)) (elems odds) in
      let mStrats = fromList [(nm, pr) | (Just nm, pr) <- strats] in
      let vStrats = map (zeroInactive mStrats) [1 .. dim odds] in
      [o / sum vStrats | o <- vStrats]
      where
        zeroInactive m nm =
          case lookup nm m of
            Just o -> o
            Nothing -> 0
extractSolution _ = error "refusing to extract solution from unsolved schema"

-- | Returns 'True' if the given 'Schema' is fully reduced (\"solved\");
-- 'False' otherwise.
solved :: Schema -> Bool
solved s =
  let ps = payoffs s in
  let ((1,1), (nr, nc)) = bounds ps in
  all (>= 0) [v | ((r, c), v) <- assocs ps, r == nr || c == nc]

-- | Keep reducing the given 'Schema' until a fixed
-- point is reached, and then return the corresponding
-- 'Solution'.
solve :: Schema -> Solution
solve s
  | solved s = extractSolution s
  | otherwise = solve (pivot s)
