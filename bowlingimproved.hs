module Main where
import Data.List as List
import Data.Vector as Vector
import Data.Bits as Bits

-- FUNCTIONS:
-- main :: IO ()
-- stripFalses :: [Bool] -> [Bool]
-- findSplitPoint :: Num t => [Bool] -> t
-- intToTF :: Integral t => t -> [Bool]
-- tfToInt :: Num a => [Bool] -> a
-- moveOne :: (Eq t, Num t) => t -> t -> [Bool]
-- moveThr :: (Num t, Ord t) => t -> t -> [Bool]
-- genOnePos :: [Bool] -> [Int]
-- genThrPos :: [Bool] -> [Int]
-- playMove :: [Bool] -> [Bool] -> [Bool]
-- mex :: (Num t, Ord t) => [t] -> t
-- memoEval :: Int -> Int
-- nthNimber :: Int -> Int
-- firstNnimbers :: Int -> Vector Int

-- writeFile "giggle.txt" (show (getDataPoints 1024))
main :: IO ()
main = let
         n = 524288 -- 6 pin case  
         dataVec = getDataPoints n -- int vector with n elems
         dataList = Vector.toList dataVec
         formatTuple :: (Int, Int) -> String
         formatTuple (p, i) = let
                                p'  = show p
                                np  = List.length p'
                                pad = 1 + (10 - np)
                                spaces 0 = []
                                spaces s = " " List.++ (spaces (s - 1)) 
                                space = spaces pad
                                i'  = show i
                              in
                                p' List.++ space List.++ i'
         dataStrList = List.map formatTuple dataList
         dataStr = List.intercalate "\n" dataStrList
       in
         writeFile "giggle.txt" dataStr

stripFalses :: [Bool] -> [Bool]
stripFalses l = List.dropWhileEnd not (List.dropWhile not l) 

findSplitPoint' :: Num t => ([Bool], t) -> t
findSplitPoint' (                [], _) = -1
findSplitPoint' (            _ : [], _) = -1
findSplitPoint' (False : False : _ , i) = i
findSplitPoint' (            _ : xs, i) = findSplitPoint' (xs, i + 1)
findSplitPoint :: Num t => [Bool] -> t
findSplitPoint l = findSplitPoint' (l, 0) 

intToTF' :: Integral t => t -> [Bool]
intToTF' (0) = []
intToTF' (n) = (1 == (n `mod` 2)) : intToTF' (n `div` 2) 
intToTF :: Integral t => t -> [Bool]
intToTF  (n) = List.reverse (intToTF' n)  

tfToInt' :: Num a => [Bool] -> a
tfToInt' ([]        ) = 0
tfToInt' (False : xs) = 0 + (2 * tfToInt' xs) 
tfToInt' (True : xs ) = 1 + (2 * tfToInt' xs)
tfToInt :: Num a => [Bool] -> a
tfToInt  (xs        ) = tfToInt' (List.reverse xs)

strToInt :: Num a => [Char] -> a
strToInt s = tfToInt (List.map charToBool s)
  where charToBool 'T' = True
        charToBool _   = False

moveOne' :: (Num t, Eq t) => t -> t -> t -> [Bool]
moveOne' s n i =  if i == n
                  then []
                  else (i /= s) : (moveOne' s n (i + 1)) 
moveOne :: (Eq t, Num t) => t -> t -> [Bool]
moveOne  s n   =  moveOne' s n 0

moveThr' :: (Ord t, Num t) => t -> t -> t -> [Bool]
moveThr' s n i =  if (i == n)
                  then []
                  else (not b) : (moveThr' s n (i + 1))
                         where b = (i - s >= 0 && i - s < 3)
moveThr :: (Num t, Ord t) => t -> t -> [Bool]
moveThr  s n   =  moveThr' s n 0

genOnePos' :: [Bool] -> Int -> [Int]
genOnePos' []  _            = []
genOnePos' (False : pins) i = genOnePos' pins (i + 1)
genOnePos' (True : pins) i  = i : genOnePos' pins (i + 1)
genOnePos :: [Bool] -> [Int]
genOnePos pins = genOnePos' pins 0

genThrPos :: [Bool] -> [Int]
genThrPos pins = if n < 2 then [] else [-1..(n-2)]
                   where n = List.length pins

boolListAnd :: [Bool] -> [Bool] -> [Bool]
boolListAnd (    []) (     _) = []
boolListAnd (     _) (    []) = []
boolListAnd (x : xs) (y : ys) = (x && y) : boolListAnd xs ys 
playMove :: [Bool] -> [Bool] -> [Bool]
playMove pins move =  boolListAnd pins move

mex' :: (Num a, Ord a) => [a] -> a
mex' (x : y : xs) = if (y > x + 1) then (x + 1) else (mex' (y : xs))
mex' (x : []    ) = x + 1
mex' ([]        ) = 0
mex :: (Num t, Ord t) => [t] -> t
mex  (x : xs) = if x > 0 then 0 else (mex' (x : xs))
mex  ([]    ) = 0

-- 2^20 is 1048576
-- 2^21 is 2097152
-- 2^22 is 4194304
-- 2^23 is 8388608
-- 2^24 is 16777216
memoEval :: Int -> Int
memoEval = (Vector.!) (Vector.map eval (Vector.generate 16777216 id))
  where eval 0 = 0
        eval m = let
                   pins = stripFalses (intToTF m)
                   splitPoint = findSplitPoint pins
                 in
                   if splitPoint /= -1
                   then let
                          (leftPins, rightPins') = List.splitAt splitPoint pins
                          rightPins = List.dropWhile not rightPins'
                          leftNimber  = memoEval (tfToInt leftPins)
                          rightNimber = memoEval (tfToInt rightPins)
                        in
                          leftNimber `Bits.xor` rightNimber 
                   else let
                          n = List.length pins
                          allOneMoves =
                            List.map (\s -> moveOne s n) (genOnePos pins)
                          allThrMoves =
                            List.map (\s -> moveThr s n) (genThrPos pins)
                          allMoves = allOneMoves List.++ allThrMoves
                          childStates = 
                            List.map (playMove pins) allMoves
                          childIndices = List.map tfToInt childStates
                        in
                          mex (List.sort (List.map memoEval childIndices))

nthNimber :: Int -> Int
nthNimber n = memoEval (tfToInt (List.replicate n True))

firstNnimbers :: Int -> Vector Int
firstNnimbers n = Vector.generate n nthNimber 

getDataPoints :: Int -> Vector (Int, Int)
getDataPoints n = Vector.generate n (\i -> (i, memoEval i))



                  









