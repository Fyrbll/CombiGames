module Main where

  import System.Environment as Env
  import Data.Vector as Vec
  import Game.Triangle as Game

  main :: IO ()
  main = Env.getArgs >>= return . transform >>= print

  transform :: [String] -> [Int]
  transform ss = 
    let
      upper = read (ss !! 0) 
      diff  = read (ss !! 1)
    in
      (Vec.toList . Vec.map Game.evaluate) 
      (Vec.generate (upper + 1) (\i -> (i, i + diff, 0)))
