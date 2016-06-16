module Main where

import HTypes
import Coding 

--Ex:
exTree = Node 0 (Leaf 'a' 0) (Node 0 (Leaf 'b' 0) (Leaf 't' 0))
--mess=[R,L,L,R,R,R,R,L,R,R]
mess = "RLLRRRRLRR"
main = do
       print $ fromStringToBits mess
       print $ decodeMessage exTree $ fromStringToBits mess
