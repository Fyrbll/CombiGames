import Data.List
import Data.Bits
import Data.Maybe
import Data.Map as Map

filterIdx' (p) ([]    ) (_) = []
filterIdx' (p) (x : xs) (i) = 
 (if p (i, x) 
  then x : (filterIdx' p xs (i + 1)) 
  else (filterIdx' p xs (i + 1))) 
filterIdx (p) (xs) = filterIdx' p xs 0 

data Game a = One a | Two (Game a, Game a) deriving Show 

findSplitPoint (                [], -1, _) = -1
findSplitPoint (            _ : [], -1, i) = -1
findSplitPoint (False : False : xs, -1, i) = i
findSplitPoint (            _ : xs, -1, i) = findSplitPoint (xs, -1, i + 1)

removeLeadingFalse (        []) = []
removeLeadingFalse (False : xs) = removeLeadingFalse xs
removeLeadingFalse (        xs) = xs

removeTrailingFalse (xs) = reverse (removeLeadingFalse (reverse xs))

allPins 0 = []
allPins i = True : (allPins (i-1)) 

moveThr' s n c =  if (c == n)
                  then []
                  else
                    (let x = (case (c - s) of
                                0 -> False
                                1 -> False
                                2 -> False
                                _ -> True)
                     in
                       x : (moveThr' s n (c + 1)))
 
moveOne' s n c =  if (c == n)
                  then []
                  else
                    (let x = (case (c - s) of
                                0 -> False
                                _ -> True)
                     in
                       x : (moveOne' s n (c + 1)))

moveThr s n = moveThr' s n 0

moveOne s n = moveOne' s n 0

elemAnd (    []) (     _) = []
elemAnd (     _) (    []) = []
elemAnd (x : xs) (y : ys) = let z = case (x, y) of
                                      (True, True) -> True
                                      (         _) -> False
                            in z : (elemAnd xs ys)

cleanUp g = removeLeadingFalse (removeTrailingFalse g)  

splitGame g = case (findSplitPoint (cleanUp g, -1, 0)) of
                (-1) -> One (cleanUp g)
                ( n) -> Two (One (cleanUp (take n g)), 
                             One (cleanUp (drop n g))) 

playMove g m = splitGame (elemAnd g m)

mex' (x : (y : xs)) = if (y > x + 1) then (x + 1) else (mex' (y : xs))
mex' (x : xs      ) = x + 1
mex' ([]          ) = 0
mex  (x : xs) = if x > 0 then 0 else (mex' (x : xs))
mex  ([]    ) = 0

possibleOneMoves n = [0..(n-1)]

possibleThrMoves n = [(-1)..(n-2)]

memo = [Just 1, Just 2, Just 3, Just 0, Just 5, Just 1, 
        Just 3, Just 0, Just 5, Just 4, Nothing, Nothing]

evaluate :: Game [Bool] -> Int
evaluate (One ([])  ) = 0
evaluate (One bs    ) = 
  (let
     n = length bs
   in
     if and bs
     then (case (memo !! (n - 1)) of
             Just m  -> m
             Nothing -> (let 
                           movesIdxOne' = possibleOneMoves n
                           movesIdxOne = 
                             filterIdx (\(i, x) -> bs !! i) movesIdxOne'
                           movesOne = 
                             Data.List.map (\s -> moveOne s n) movesIdxOne
                           movesIdxThr = possibleThrMoves n 
                           movesThr = 
                             Data.List.map (\s -> moveThr s n) movesIdxThr
                           moves = movesOne ++ movesThr
                           results = Data.List.map (playMove bs) moves
                           nimber = 
                             mex (Data.List.sort (Data.List.map evaluate results))
                         in
                           nimber))
     else (let 
             movesIdxOne' = possibleOneMoves n
             movesIdxOne = filterIdx (\(i, x) -> bs !! i) movesIdxOne'
             movesOne = Data.List.map (\s -> moveOne s n) movesIdxOne
             movesIdxThr = possibleThrMoves n 
             movesThr = Data.List.map (\s -> moveThr s n) movesIdxThr
             moves = movesOne ++ movesThr
             results = Data.List.map (playMove bs) moves
             nimber = mex (Data.List.sort (Data.List.map evaluate results)) 
           in
             nimber))
evaluate (Two (g1, g2)) = (evaluate g1) `Data.Bits.xor` (evaluate g2)

-- intToTF :: Int -> [Bool]
intToTF' (0) = []
intToTF' (n) = (1 == (n `mod` 2)) : intToTF' (n `div` 2) 
intToTF  (n) = reverse (intToTF' n)  

-- tfToInt :: [Bool] -> Int
tfToInt' ([]        ) = 0
tfToInt' (False : xs) = 0 + (2 * tfToInt' xs) 
tfToInt' (True : xs ) = 1 + (2 * tfToInt' xs)
tfToInt  (xs        ) = tfToInt' (reverse xs)

-- mapi
mapi f xs = Data.List.map f (zip [0..(length xs - 1)] xs)

-- defining source list for memoization
source = Data.List.map (\i -> (One (intToTF i), Nothing)) [1..] 

-- extracting Nimbers
extractNimber :: (Game [Bool], Maybe Int) -> Int
extractNimber (_, Nothing) = -1
extractNimber (_, Just v ) = v

-- revised evaluate:
memoEval = \i -> (Data.List.map eval source) !! i
  where eval (One [],   _) = (One [], Just 0)
        eval (One pins, _) = 
          let
            n = length pins 
            splitPoint = findSplitPoint (pins, -1, 0)
          in
            if splitPoint > -1
            then (One pins, Nothing)
            else 
              case memoEval (tfToInt pins - 1) of
                (_, Just v ) -> (One pins, Just v)
                (_, Nothing) -> 
                  let 
                    movesIdxOne' = possibleOneMoves n
                    movesIdxOne = filterIdx (\(i, x) -> pins !! i) movesIdxOne'
                    movesOne = Data.List.map (\s -> moveOne s n) movesIdxOne
                    movesIdxThr = possibleThrMoves n 
                    movesThr = Data.List.map (\s -> moveThr s n) movesIdxThr
                    moves = movesOne ++ movesThr
                    results' = Data.List.map (playMove pins) moves
                    results = Data.List.map (\x -> (x, Nothing)) results'
                    subNimbers' = Data.List.map eval results
                    subNimbers = Data.List.map extractNimber subNimbers'
                    nimber = mex (Data.List.sort subNimbers) 
                  in
                    (One pins, Just nimber)
        eval (Two (g1, g2), _) = 
          case (eval (g1, Nothing), eval (g2, Nothing)) of
            ((_, Just v1), (_, Just v2)) -> (One [], Just (v1 `xor` v2))
            (_                         ) -> (One [], Nothing)
              

memoEval' = \i -> (Data.List.map eval source) !! i
  where eval (One [],   _) = (One [], Just 0)
        eval (One pins, _) = 
          let
            n = length pins 
            splitPoint = findSplitPoint (pins, -1, 0)
          in
            if splitPoint > -1
            then (One pins, Nothing)
            else 
              let 
                movesIdxOne' = possibleOneMoves n
                movesIdxOne = filterIdx (\(i, x) -> pins !! i) movesIdxOne'
                movesOne = Data.List.map (\s -> moveOne s n) movesIdxOne
                movesIdxThr = possibleThrMoves n 
                movesThr = Data.List.map (\s -> moveThr s n) movesIdxThr
                moves = movesOne ++ movesThr
                results' = Data.List.map (playMove pins) moves
                results = Data.List.map (\x -> (x, Nothing)) results'
                midEval (One [], _) = (One [], Just 0)
                midEval (One bs, _) = memoEval' (tfToInt bs - 1)
                midEval (t     , u) = eval (t, u) 
                subNimbers' = Data.List.map midEval results
                subNimbers = Data.List.map extractNimber subNimbers'
                nimber = mex (Data.List.sort subNimbers) 
              in
                (One pins, Just nimber)
        eval (Two (g1, g2), _) = 
          case (eval (g1, Nothing), eval (g2, Nothing)) of
            ((_, Just v1), (_, Just v2)) -> (One [], Just (v1 `xor` v2))
            (_                         ) -> (One [], Nothing)
