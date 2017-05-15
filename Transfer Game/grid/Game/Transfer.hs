module Game.Transfer  
( setup -- Int -> Game
, moveList -- Game -> Seq.Seq Move
, makeMove -- Game -> Move -> Game
, evaluate -- Game -> Int
, validState -- Int -> Game -> Bool
, intToGame -- Int -> Int -> Game
, evalChildren -- Game -> Seq.Seq (Game, Int)
) where

  import Data.Sequence as Seq
  import Data.Vector as Vec

  type Game = (Int, Int, Int)
  type Move = (Int, Int)

  setup :: Int -> Game
  setup n = (0, n, 0)

  moveList :: Game -> Seq.Seq Move
  moveList (_, 0, _) = Seq.empty
  moveList (l, m, r) = 
    let
      maxLeft = min ((m - l) `div` 2) (m - r)
      maxRght = min ((m - r) `div` 2) (m - l)
      leftMoves = Seq.fromFunction maxLeft (\n -> (n + 1, 1))
      rghtMoves = Seq.fromFunction maxRght (\n -> (n + 1, 3))
    in
      leftMoves Seq.>< rghtMoves

  makeMove :: Game -> Move -> Game
  makeMove (l, m, r) (n, 1) = (l + n, m - n, r)
  makeMove (l, m, r) (n, 3) = (l, m - n, r + n)
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

  mex :: Seq.Seq Int -> Int
  mex s = if Seq.length s > 0 && Seq.index s 0 >= 1 then 0 else mex' s
    where
      mex' t = case Seq.length t of
                 0 -> 0
                 1 -> Seq.index t 0 + 1
                 _ -> if Seq.index t 0 + 2 == Seq.index t 1
                      then Seq.index t 0 + 1
                      else mex' (Seq.drop 1 t)

  evalChildren :: Game -> Seq.Seq (Game, Int)
  evalChildren g = fmap ((\x -> (x, evaluate x)) . makeMove g) (moveList g)
