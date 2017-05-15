module Main where

  import Text.CSV as Csv
  import System.Environment as Env
  import Game.Chess as Chess

  {-| When run with the command line argument n, makes an (n x n) table
   -  and prints it, in CSV format, to Nimbers/results.csv
   -}
  main :: IO ()
  main = Env.getArgs >>= 
         return . makeTable >>= 
         writeFile "../results/nimbers.csv"

  makeTable :: [String] -> String
  makeTable args =
    let 
      n = read (args !! 0)
    in
      printCSV
      (map (\r -> map (\c -> show (Chess.evaluate (r,c))) [0..n]) [0..n])
