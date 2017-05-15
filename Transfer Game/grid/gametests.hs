module Main where

import qualified Test.HUnit as HUnit
import qualified Data.Sequence as Seq
import qualified Game.Triangle as Tri

main :: IO ()
main = test1 >> test2 >> test3 >> test4 >> test5 >> test6 >> test7 >> test8 >>
       testA >> testB >> testC >> testD >> testE

testg :: Tri.Game -> [Tri.Game] -> IO ()
testg g c = HUnit.assertEqual 
            ("Children of " ++ show g ++ ":")
            (fmap (Tri.makeMove g) (Tri.moveList g))
            (Seq.fromList c)

test1 = testg (0,10,0) [(1,9,0),(2,8,0),(3,7,0),(4,6,0),(5,5,0)]
test2 = testg (1,9,0) [(2,8,0),(3,7,0),(4,6,0),(5,5,0),
                       (1,8,1)]
test3 = testg (2,8,0) [(3,7,0),(4,6,0),(5,5,0),
                       (2,7,1),(2,6,2),
                       (1,8,1)]
test4 = testg (3,7,0) [(4,6,0),(5,5,0),
                       (3,6,1),(3,5,2),(3,4,3),
                       (2,7,1)]
test5 = testg (4,6,0) [(5,5,0),
                       (4,5,1),(4,4,2),
                       (3,6,1),(2,6,2)]
test6 = testg (5,5,0) [(4,5,1),(3,5,2)]
test7 = testg (1,8,1) [(2,7,1),(3,6,1),(4,5,1)]
test8 = testg (2,7,1) [(3,6,1),(4,5,1),
                       (2,6,2)]
test9 = testg (2,6,2) [(3,5,2),(4,4,2)]
testA = testg (3,6,1) [(4,5,1),
                       (3,5,2),(3,4,3),
                       (2,6,2)]
testB = testg (3,4,3) []
testC = testg (4,5,1) [(4,4,2),
                       (3,5,2)]
testD = testg (4,4,2) [(3,4,3)]
testE = testg (3,5,2) [(4,4,2),
                       (3,4,3)]
