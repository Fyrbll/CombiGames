module Game.Triangle
( setup -- Int -> Game
, moveList -- Game -> Seq.Seq Move
, makeMove -- Game -> Move -> Game
, evaluate -- Game -> Int
, validState -- Int -> Game -> Bool
, intToGame -- Int -> Int -> Game
, evalChildren -- Game -> Seq.Seq (Game, Int)
, zeroize -- (Game, Int) -> (Game, Int)
, Game
) where

import Data.Sequence as Seq
import Data.Vector as Vec
import Data.Ord as Ord
import Data.List as L
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe

type Game = (Int, Int, Int)
type Move = (Int, Int, Int)

setup :: Int -> Game
setup n = (0, n, 0)

moveList :: Game -> Seq.Seq Move
moveList (_, 0, _) = Seq.empty
moveList (l, m, r) = 
  let
    (maxLeft, maxRght) = 
      case Ord.compare l r of
        LT -> (L.foldl min m [r - l, (m - l) `div` 2, m - r], (m - r) `div` 2)
        EQ -> ((m - l) `div` 2, 0)
        GT -> ((m - l) `div` 2, L.foldl min m [l - r, (m - r) `div` 2, m - l])
    leftMoves = Seq.fromFunction maxLeft (\n -> (n + 1, 2, 1))
    rghtMoves = Seq.fromFunction maxRght (\n -> (n + 1, 2, 3))
    baseMoves = 
      case compare l r of
        LT -> Seq.fromFunction ((r - l) `div` 2) (\n -> (n + 1, 3, 1)) 
        GT -> Seq.fromFunction ((l - r) `div` 2) (\n -> (n + 1, 1, 3))
        EQ -> Seq.empty
  in
    leftMoves Seq.>< rghtMoves Seq.>< baseMoves

makeMove :: Game -> Move -> Game
makeMove (l, m, r) (n, 2, 1) = (l + n, m - n, r)
makeMove (l, m, r) (n, 3, 1) = (l + n, m, r - n)
makeMove (l, m, r) (n, 1, 3) = (l - n, m, r + n)
makeMove (l, m, r) (n, 2, 3) = (l, m - n, r + n)
makeMove g         _      = g

evaluate :: Game -> Int
evaluate g@(l, m, r) = 
  let
    t = l + m + r
    memoEval = (Vec.!) (Vec.map eval (Vec.generate ((t + 1) ^ (3 :: Int)) id))
      where
        eval i = let g' = intToGame t i
                 in if validState t g'
                    then mex (Seq.unstableSort 
                             (fmap (memoEval . gameToInt t . makeMove g')
                             (moveList g')))
                    else (-1)
  in memoEval (gameToInt t g)

validState :: Int -> Game -> Bool
validState n (l, m, r) = m >= l && m >= r && m + l + r == n

intToGame :: Int -> Int -> Game
intToGame n i = (l, m, r) where b = n + 1
                                (qr, r) = (i  `div` b, i  `mod` b)
                                (qm, m) = (qr `div` b, qr `mod` b)
                                l = qm `mod` b 

gameToInt :: Int -> Game -> Int
gameToInt n (l, m, r) = let b = n + 1 in l * b ^ (2 :: Int) + m * b + r

evalChildren :: Game -> Seq.Seq (Game, Int)
evalChildren g = fmap ((\x -> (x, evaluate x)) . makeMove g) (moveList g)

zeroize :: (Game, Int) -> (Game, Int)
zeroize ((0,m,r),n) = ((0,m,r),n)
zeroize ((l,m,0),n) = ((0,m,l),n)
zeroize ((l,m,r),n) = if l >= r then ((0,m-r,l-r),n) else ((0,m-l,r-l),n)

binarySearch :: Vec.Vector Int -> Int -> Maybe Int
binarySearch v t =
  let
    binarySearchHelper l h = 
      if h < l
      then Nothing
      else let m = (l + h) `div` 2 
           in case compare t (v Vec.! m) of
                LT -> binarySearchHelper l (m - 1)
                EQ -> Just m
                GT -> binarySearchHelper (m + 1) h
  in
    binarySearchHelper 0 (Vec.length v - 1)

mex :: Seq.Seq Int -> Int
mex s =
  let
    mexHelper s t =
      case Seq.viewl s of
        Seq.EmptyL    -> t
        (x Seq.:< xs) -> case compare x t of
                           LT -> mexHelper xs t
                           EQ -> mexHelper xs (t + 1)
                           GT -> t
  in
    mexHelper s 0

memoFib :: Integer -> Integer
memoFib n =
  let
    fib m i
     | i > n     = Maybe.fromJust (Map.lookup n m)
     | i == 0    = let m' = Map.insert i 0 m in fib m' (i + 1)
     | i == 1    = let m' = Map.insert i 1 m in fib m' (i + 1)
     | otherwise = let
                     a  = Maybe.fromJust (Map.lookup (i - 1) m)
                     b  = Maybe.fromJust (Map.lookup (i - 2) m)
                     m' = Map.insert i (a + b) m
                   in
                     fib m' (i + 1)
  in
    fib Map.empty 0
    
