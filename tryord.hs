import Control.Exception.Base
import Data.Map as Map

-- Gamestate Datatype
data GS = GS [Bool] deriving Show  

instance Eq GS where
  (==) (GS []) (GS []) = True
  (==) (GS []) (_ ) = False
  (==) (_ ) (GS []) = False
  (==) (GS (x : xs)) (GS (y : ys)) = (x == y) && ((GS xs) == (GS ys)) 

instance Ord GS where
  compare (GS []) (GS []) = EQ
  compare (GS []) (_ ) = GT
  compare (_ ) (GS []) = LT
  compare (GS (True : xs) ) (GS (False : ys)) = GT
  compare (GS (False : xs)) (GS (True : ys) ) = LT
  compare (GS (x : xs)    ) (GS (y : ys)    ) = compare (GS xs) (GS ys)

-- int2bin conversion
intToBin' (0) = []
intToBin' (n) = (n `mod` 2) : intToBin' (n `div` 2) 
intToBin (n) = reverse (intToBin' n)  

--int2TF conversion
intToTF' (0) = []
intToTF' (n) = (1 == (n `mod` 2)) : intToTF' (n `div` 2) 
intToTF  (n) = reverse (intToTF' n)  

--TF2int conversion
tfToInt' ([]        ) = 0
tfToInt' (False : xs) = 0 + (2 * tfToInt' xs) 
tfToInt' (True : xs ) = 1 + (2 * tfToInt' xs)
tfToInt  (xs        ) = tfToInt' (reverse xs)

--check for 2 continuous 'False's
findSplitPoint (                [], -1, _) = -1
findSplitPoint (            _ : [], -1, i) = -1
findSplitPoint (False : False : xs, -1, i) = i
findSplitPoint (            _ : xs, -1, i) = findSplitPoint (xs, -1, i + 1)

-- Sandbox
memoMap0 = Map.empty
memoMap1 = Map.insert (GS [True, True]) (2) (memoMap0)
memoMap2 = Map.insert (GS [True, True, True]) (3) (memoMap1)
memoMap3 = Map.insert (GS [True]) (1) (memoMap2)

-- Tests 
t1 = assert (compare (GS [True, False]) (GS [True, True]) == LT) (0)
