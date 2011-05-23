-- Copyright © 2011 Bart Massey

-- | Format fields in a tabular fashion.

module Text.Tabular (tabular)
where

import Data.List

-- | Given a format string whose characters are 'l', 'r' and
--   'c' for left-justified, right-justified and centered
--   columns, and a list of rows, each row consisting of
--   columns of strings to be formatted, emit a
--   properly-formatted matrix. Short rows will be padded to
--   the size of the longest row with empty strings before
--   the data is applied to the format string, at which
--   point the number of columns per row must match.
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
