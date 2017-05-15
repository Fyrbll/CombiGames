module Sequences.Pattern where

import qualified Data.List as L

findPattern :: [Int] -> [Int]
findPattern seq = findPattern' 1 seq

findPattern' :: Int -> [Int] -> [Int]
findPattern' test_len seq =
  let
    seq_len = L.length seq
  in
    if test_len == seq_len
    then seq
    else
      let
        chunks_len = seq_len `div` test_len
        test = L.take test_len seq
        chunks = 
          L.map (\i -> L.take test_len (L.drop (i * test_len) seq)) 
          [0..(chunks_len - 1)]
        test_repeat = L.replicate chunks_len test
        zipped = L.zip chunks test_repeat
        repeat_count = 
          L.foldl (\c (l1, l2) -> if l1 == l2 then c + 1 else c) 0 zipped
      in
        if repeat_count >= (chunks_len - 1)
        then test 
        else findPattern' (test_len + 1) seq
