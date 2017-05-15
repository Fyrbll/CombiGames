module Sequences.OldPattern where

import qualified Data.List as L

findPattern :: Int -> [Int] -> [Int]
findPattern l xs =
  let
    p = L.take l xs
    c = countPattern p xs
  in
    if c < 2
    then remRepeats 1 (L.init p)
    else findPattern (l+1) xs

countPattern :: [Int] -> [Int] -> Int
countPattern p xs =
  let
    np  = L.length p 
    nxs = L.length xs
    d   = nxs - np
  in
    if d < 0
    then 0
    else
      let
        ss = L.map (\i -> L.take np (L.drop i xs)) [0..d]
        pr = L.replicate (d + 1) p
        spz = zip ss pr
      in
        foldl (\c (l1, l2) -> if l1 == l2 then c + 1 else c) 0 spz

remRepeats :: Int -> [Int] -> [Int]
remRepeats l xs =
  let
    nxs = L.length xs
  in
    if l == nxs
    then xs
    else
      let
        n = nxs `div` l
        t = L.take l xs
        s = L.map (\i -> L.take l (L.drop (i * l) xs)) [0..(n-1)]
        r = L.replicate n t
        z = L.zip s r
        c = L.foldl (\c (l1, l2) -> if l1 == l2 then c+1 else c) 0 z
      in
        if c == n then t else remRepeats (l+1) xs
