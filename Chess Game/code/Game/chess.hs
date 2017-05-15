module Game.Chess (evaluate) where

  import qualified Data.Vector   as Vec
  import qualified Data.Sequence as Seq
  import qualified Data.List     as Lst

  type Nimber = Int
  type Row    = Int
  type Col    = Int
  type Game   = (Row, Col)
  data Move   = LEFT Int | UP Int

  move_list :: Game -> Seq.Seq Move
  move_list (row, col) =
    Seq.fromFunction (min row (col `div` 2)) (\i -> LEFT (i + 1)) Seq.><
    Seq.fromFunction (min (row `div` 2) col) (\i -> UP   (i + 1))

  make_move :: Game -> Move -> Game
  make_move (row, col) (LEFT n) = (row - n    , col - 2 * n)
  make_move (row, col) (UP   n) = (row - 2 * n, col - n    )

  evaluate :: Game -> Nimber
  evaluate game@(row, col) = 
    let max_idx = (row + 1) * (col + 1)
        memo_eval = (Vec.!) (Vec.map eval_help (Vec.generate max_idx id))
          where
            eval_help idx = let game = idx_to_game row col idx
                            in if valid_state game
                               then (mex .
                                     Seq.unstableSort .
                                     fmap (memo_eval . 
                                           game_to_idx row col .
                                           make_move game))
                                    (move_list game)
                               else -1
    in memo_eval (game_to_idx row col game)

  valid_state :: Game -> Bool
  valid_state (row, col) = (row >= 0) && (col >= 0)

  idx_to_game :: Row -> Col -> Int -> Game
  idx_to_game _ max_col idx = (idx `div` (max_col + 1), 
                               idx `mod` (max_col + 1)) 

  game_to_idx :: Row -> Col -> Game -> Int
  game_to_idx _ max_col (row, col) = row * (max_col + 1) + col

  mex :: Seq.Seq Nimber -> Nimber
  mex s = if Seq.length s > 0 && Seq.index s 0 >= 1 then 0 else mex' s
    where
      mex' t = case Seq.length t of
                 0 -> 0
                 1 -> Seq.index t 0 + 1
                 _ -> if Seq.index t 0 + 2 == Seq.index t 1
                      then Seq.index t 0 + 1
                      else mex' (Seq.drop 1 t)
