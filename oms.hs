-- Copyright © 2011 Bart Massey

import Data.Array
import OptimalMixedStrategy

untilM :: Monad m => (v -> Bool) -> (v -> m v) -> v -> m v
untilM p _ v | p v = return v
untilM p a v = a v >>= untilM p a

solved :: Schema -> Bool
solved s =
  let ps = payoffs s in
  let ((1,1), (nr, nc)) = bounds ps in
  all (>= 0) [v | ((r, c), v) <- assocs ps, r == nr || c == nc]

step :: Schema -> IO Schema
step s = do
  let s' = pivot s
  print s'
  return s'

main :: IO ()
main = do
  s0 <- readSchema
  print s0
  s <- untilM solved step s0
  print $ extractSoln s
