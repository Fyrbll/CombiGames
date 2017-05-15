module Main where

  import Graphics.Gloss
  import System.Environment

  -- Vector containing info

  main :: IO ()
  main = 
    getArgs >>= 
    return . transform >>=
    
  


  transform :: [String] -> (Display, Picture)
  transform args =
    let
      maxStones = read (args !! 0) 
      side = 20
      size = side * maxStones
      window = InWindow "Grid Window" (size,size) (10, 10)
      grid = concat (map 
             (\r -> map (\c -> (c * side, r * side)) [0..maxStones-1]) 
             [0..maxStones-1])
      pics = pictures 
             (map (\(l,t) -> square (size,size) (fromIntegral l, fromIntegral t) 
                             (fromIntegral side)) grid)
    in
      (window, pics)

  square :: (Int, Int) -> Point -> Float -> Picture
  square (width,height) (top,left) side =
    let
      top' = (fromIntegral height :: Float) / 2 - top
      left' = left - (fromIntegral width :: Float) / 2
      path = [(left',top'),
              (left'+side,top'),
              (left'+side,top'-side),
              (left',top'-side)]
    in
      pictures [color white (polygon path), 
                color black (line path), 
                translate (left'+0.25*side) 
                          (top'-0.75*side) 
                          (scale 0.1 0.1 (text "1"))]
