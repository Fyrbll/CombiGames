module Main where

import qualified Sequences.Pattern as P
import qualified Data.List         as L

source :: [Int]
source = [6,1,1,1,-4,6,1,1,1,-4,5,2,1,1,1,-4,6,1,1,1,-4,6,1,1,1,-4,5,2,1,1,1,-4,6,1,1,1,-4,6,1,1,1,-4,5,2,1,1,1,-4,6,1,1,1,-4,6,1,1,1,-4,5,2,1,1,1,-4,6,1,1,1,-4,6,1,1,1,-4,5,2,1,1,1,-4,6,1,1,1,-4,6,1,1,1,-4,5,2,1,1,1,-4,6,1,1]

main :: IO ()
main = main' source

main' :: [Int] -> IO ()
main' x = print (reverse (P.findPattern (reverse x)))

patt :: [Int]
patt = reverse (P.findPattern (reverse source))
