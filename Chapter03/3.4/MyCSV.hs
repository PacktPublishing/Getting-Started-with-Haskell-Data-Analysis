module MyCSV where

import Text.CSV

noEmptyRows :: Either a CSV -> CSV
noEmptyRows ecsv = either (const []) (filter (\row -> 2 <= length row)) ecsv

{- Reads a column of data from a CSV file -}
readIndex :: Read cell => Either a CSV -> Int -> [cell]
readIndex ecsv index = map read (getIndex ecsv index)

{- Reads a column of data from a CSV file -}
getIndex :: Either a CSV -> Int -> [String]
getIndex ecsv index = map (!! index) (noEmptyRows ecsv)
