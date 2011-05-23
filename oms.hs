-- Copyright © 2011 Bart Massey

import OptimalMixedStrategy

untilM :: Monad m => (v -> Bool) -> (v -> m v) -> v -> m v
untilM p _ v | p v = return v
untilM p a v = a v >>= untilM p a

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
