-- Copyright © 2011 Bart Massey

module Text.Tabular
where

import Data.List

-- | Format fields in a tabular fashion.

tabular :: String -> [[String]] -> String
tabular fmt tableau =
  let rawCols = 
        transpose $ padRows tableau
        where 
          padRows rows =
            map (padRow $ maximum $ map length rows) rows
            where
              padRow l row =
                row ++ replicate (l - length row) ""
      in
  let padCols = 
        padCol fmt rawCols
        where
          padCol (c : cs) (col : cols)
            | c `elem` "lrc" =
              let w = maximum $ map length col
                  cols' = padCol cs cols
                  in
              let Just padf = 
                    lookup c [
                      ('l', padLeft),
                      ('r', padRight),
                      ('c', padCenter) ]
                    where
                      padLeft s = 
                        s ++ replicate (w - length s) ' '
                      padRight s = 
                        replicate (w - length s) ' ' ++ s
                      padCenter s =
                        let dw = w - length s in
                        let hdw = dw `div` 2 in
                        replicate hdw ' ' ++ s ++ replicate (dw - hdw) ' '
                  in
              map padf col : cols'
          padCol "" [] = []
          padCol _ _ = error "tabular data error"
      in
   unlines $ map unwords $ transpose padCols
