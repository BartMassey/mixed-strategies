-- Copyright © 2011 Bart Massey

-- Calculate an optimal mixed strategy given
-- the payoff matrix of a two-player zero-sum
-- single-round iterated simultaneous game.
-- Follows the method of Chapter 6 of
--   J.D. Williams. The Compleat Strategist. McGraw-Hill 1954. 

import Prelude hiding (Left, Right, lookup)
import Data.Array
import Data.Function (on)
import Data.List hiding (lookup)
import Data.Map (lookup, fromList)
import Data.Ord (comparing)
import Text.Printf
import Text.Tabular

dim :: Array Int t -> Int
dim a = let (1, ix) = bounds a in ix

dims :: Array (Int, Int) t -> (Int, Int)
dims a = let ((1, 1), ixs) = bounds a in ixs

data Edge = Left | Top | Right | Bottom deriving (Ord, Eq, Ix)

data Schema = Schema {
  offset :: Double,
  d :: Double,
  names :: Array Edge (Array Int (Maybe Int)),
  payoffs :: Array (Int, Int) Double
}

instance Show Schema where
  show s =
    printf "O = %g, D = %g\n" (offset s) (d s) ++
    tabular ("r" ++ replicate (nc + 1) 'l') $
      ["" : printNameVec Top] ++
      map printRow [1 .. nr - 1] ++
      ["" : printRowM nr] ++
      ["" : printNameVec Bottom]
    where
      (nr, nc) = dims $ payoffs s
      printRowM i =
        printVec $ ixmap (1, nc) (\j -> (i, j)) (payoffs s)
        where
          printVec a =
            map (printf "%.2f" . (a !)) [1 .. dim a]
      printName e i =
        case names s ! e ! i of
          Just v -> printf "%2d" v
          Nothing -> ""
      printNameVec e =
        map (printName e) [1 .. dim (names s ! e)]
      printRow i =
        [printName Left i] ++ 
        printRowM i ++ 
        [printName Right i]

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

untilM :: Monad m => (v -> Bool) -> (v -> m v) -> v -> m v
untilM p _ v | p v = return v
untilM p a v = a v >>= untilM p a

solved :: Schema -> Bool
solved s =
  let ps = payoffs s in
  let (nr, nc) = dims ps in
  all (>= 0) [v | ((r, c), v) <- assocs ps, r == nr || c == nc]

step :: Schema -> IO Schema
step s = do
  let s' = pivot s
  print s'
  return s'

data Soln = Soln {
  value :: Double,
  leftStrategy, topStrategy :: [Double] }

instance Show Soln where
  show soln =
    printf "v = %g\n" (value soln) ++
    printf "l = %s\n" (show (leftStrategy soln)) ++
    printf "t = %s" (show (topStrategy soln))

extractSoln :: Schema -> Soln
extractSoln s = Soln {
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


main :: IO ()
main = do
  s0 <- readSchema
  print s0
  s <- untilM solved step s0
  print $ extractSoln s
