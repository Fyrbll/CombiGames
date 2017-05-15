module Main where

import qualified Sequences.Pattern    as P
import qualified Test.HUnit           as HUnit 
import qualified Data.List            as L

main :: IO ()
main = test1 >> test2 >> test3 >> test4 >> test5

test1 :: IO ()
test1 = testgen [1,1,1,1] [1]

test2 :: IO ()
test2 = testgen [1,2,1,2,3] [1,2]

test3 :: IO ()
test3 = testgen [1,4,2,1,4,2,3,4] [1,4,2]

test4 :: IO ()
test4 = testgen
        (L.reverse [7,-1,2,1,-4,5,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,
                    3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,
                    1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,
                    3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2,1,-2,3,2])
        [2,3,-2,1]

test5 :: IO ()
test5 = testgen
        (L.reverse [8,-6,7,1,-3,6,-1,2,-3,4,3,1,1,-4,1,4,2,2,-1,-2,
                    5,1,2,-4,6,1,-4,6,-4,5,1,-3,6,-1,2,-3,4,3,1,1,-4,
                    1,4,2,2,-1,-2,5,1,2,-4,6,1,-4,6,-4,5,1,-3,6,-1,2,
                    -3,4,3,1,1,-4,1,4,2,2,-1,-2,5,1,2,-4,6,1,-4,6,-4,
                    5,1,-3,6,-1,2,-3,4,3,1,1,-4,1,4,2,2])
        [2,2,4,1,-4,1,1,3,4,-3,2,-1,6,-3,1,5,-4,6,-4,1,6,-4,2,1,5,-2,-1]

testgen :: [Int] -> [Int] -> IO ()
testgen sequence expected_pattern = 
  HUnit.assertEqual "TEST:" expected_pattern (P.findPattern sequence)
