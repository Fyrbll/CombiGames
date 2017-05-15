module Main where

  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import System.Environment
  import Game.Triangle
  import qualified Data.Sequence as Seq
  import qualified Data.Foldable as Fld

  type Model = [[(Float, Float, String, Color)]]
  type PlayUncurried = (Display,
                        Color,
                        Int,
                        Model,
                        (Model -> Picture),
                        (Event -> Model -> Model),
                        (Float -> Model -> Model))

  main :: IO ()
  main = getArgs >>= return . transform >>= play'
         
  play' :: PlayUncurried -> IO ()
  play' (w, bg, fps, m, v, c, a) = play w bg fps m v c a

  transform :: [String] -> PlayUncurried
  transform args =
    let
      -- Preliminary
      maxStones = read (args !! 0)
      side = 25
      size = side * maxStones
      extra = 20
      -- Display
      w = InWindow "Nimber Grid" (size,size) (10, 10)
      -- Color
      bg = white
      -- Int
      fps = 2
      -- Model
      fillSquare side r c = (fromIntegral (c * side) :: Float,
                             fromIntegral (r * side) :: Float,
                             show (evaluate (0,r,c)),
                             white)
      m = seq () (map
          (\r -> map
                 (\c -> fillSquare side r c) 
                 [0..maxStones-1]) 
          [0..maxStones-1])
      -- Model -> Picture
      drawSquare size side (l, t, n, c) = 
        let
          side' = fromIntegral side :: Float
          t' = (fromIntegral size :: Float) / 2 - t
          l' = l - (fromIntegral size :: Float) / 2
          path = [(l',t'),
                  (l'+side',t'),
                  (l'+side',t'-side'),
                  (l',t'-side')]
        in
          pictures [color c (polygon path), 
                    color black (line path), 
                    translate (l'+0.25*side') 
                              (t'-0.75*side') 
                              (scale 0.1 0.1 (text n))]
      v m = (pictures . map (drawSquare size side)) (concat m)
      -- Event -> Model -> Model
      whitewash m = map (\r -> map (\(x, y, n, c) -> (x, y, n, white)) r) m
      mapi f l = map f (zip l [0..(length l)-1])
      highlight (r,c) m =
        let
          g (x, i) = if i == c then let (xx,yy,nn,cc) = x in (xx,yy,nn,red)
                     else id x
          f (x, i) = if i == r then mapi g x else id x
        in
          mapi f m
      c (EventKey (MouseButton LeftButton) Down _ (x,y)) m =
        let
          x' = 0.5 * (fromIntegral size :: Float) + x 
          y' = 0.5 * (fromIntegral size :: Float) - y 
          r = floor (y' / (fromIntegral side :: Float)) :: Int
          c = floor (x' / (fromIntegral side :: Float)) :: Int
          hlList = map (\((_,m,r),_) -> (m,r))
                   (Fld.toList (fmap zeroize (evalChildren (0,r,c))))
          highlight' ([]  ) m = m 
          highlight' (x:xs) m = highlight' xs (highlight x m)
        in
          (highlight' ((r,c):hlList) . whitewash) m
      c _ m = m
      -- Float -> Model -> Model
      a _ m = m
    in
      (w, bg, fps, m, v, c, a)

