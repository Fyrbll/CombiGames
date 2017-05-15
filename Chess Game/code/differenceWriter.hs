module Main where

  import Text.CSV as Csv
  import System.Environment as Env
  import Game.Chess as Chess

  {-| A multiline comment --- describe the function here
   -}
  main :: IO ()
  main = Env.getArgs >>= 
         return . make_differences >>=
         write_string_to_file

  make_differences :: [String] -> (String, String)
  make_differences args =
    let 
      (row, n_terms) = (read (args !! 0), read (args !! 1))
      differences list = map (uncurry (-)) (zip (drop 1 list) list)
      idx_to_nimber idx = Chess.evaluate (row + idx, row + 2 * idx)
    in
      (printCSV [(map show (differences (map idx_to_nimber [0..n_terms - 1])))],
       "../results/differences-" ++ (show row) ++ ".csv")

  write_string_to_file :: (String, String) -> IO ()
  write_string_to_file (contents, filename) = writeFile filename contents 
