module Main where

import Game.Transfer as G
import Data.Vector as V
import Data.List as L

main :: IO ()
main =
  let
    centralStack = 28
    neededNimber = 0
    fileName = "Nimbers/transferNimbers28-0.txt"
    stateNimberToString ((l, m, r), n) =
      let
        spacePerNumber = (L.length . show) centralStack
        numberToString i =
          let
            iString = show i
            iStringSize = L.length iString
          in
            if iStringSize < spacePerNumber
            then L.replicate (spacePerNumber - iStringSize) ' ' L.++ iString
            else iString
      in
        "(" L.++ numberToString l L.++ "," L.++ numberToString m L.++
        "," L.++ numberToString r L.++ ") --> " L.++ show n L.++ "\n"
    stateVector = 
      V.filter (G.validState centralStack) 
      (V.map (G.intToGame centralStack) 
      (V.generate ((centralStack + 1) ^ (3 :: Int)) id))
    stateNimberVector =
      V.zip stateVector (V.map G.evaluate stateVector)
    filteredStateNimberVector =
      V.filter (\(_, n) -> n == neededNimber) stateNimberVector
    filteredStateNimberVectorString =
      (unwords . V.toList . V.map stateNimberToString) filteredStateNimberVector
  in
    writeFile fileName filteredStateNimberVectorString
