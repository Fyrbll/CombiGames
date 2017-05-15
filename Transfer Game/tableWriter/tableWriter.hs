module Main where

  import Text.CSV as Csv
  import System.Environment as Env
  import Game.Triangle as Tri

  {-| When run with the command line argument n, makes an (n x n) table
   -  and prints it, in CSV format, to Nimbers/results.csv
   -}
  main :: IO ()
  main = Env.getArgs >>= return . makeTable >>= writeFile "Nimbers/results.csv"

  makeTable :: [String] -> String
  makeTable args =
    let n = read (args !! 0) in
      printCSV (map 
          (\i -> map (\j -> show (Tri.evaluate (0,i,j))) [0..n]) 
        [0..n])
      

    
