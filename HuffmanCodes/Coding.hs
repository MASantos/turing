module Coding (codeMessage, 
               decodeMessage ) where

import HTypes ( Tree(Leaf,Node), Bit(L,R), HCode, Table )

codeMessage :: Table -> [Char] -> HCode
codeMessage tb = concat . map (lookupTable tb)

lookupTable :: Table -> Char -> HCode
lookupTable [] c = error "Blank Coding table."
lookupTable ((ch,n):tb) c 
                       | ch==c = n
                       | otherwise = lookupTable tb c


decodeMessage:: Tree -> HCode -> [Char]
decodeMessage tr = decodeByt tr where
                   decodeByt (Node n t1 t2) (L:rest) = decodeByt t1 rest
                   decodeByt (Node n t1 t2) (R:rest) = decodeByt t2 rest
                   decodeByt (Leaf c n) rest = c : decodeByt tr rest
                   decodeByt _ [] = []
--Compiles but loops forever printing just b's for 
-- tree /\ 
--     a /\
--       b t
-- code = RLLRRRRLRR
--decodeMessage _ [] = []
--decodeMessage tr code =  case (tr,code) of     
--                ((Node n t1 t2),(L:rest)) -> decodeMessage t1 rest
--                ((Node n t1 t2),(R:rest)) -> decodeMessage t2 rest
--                ((Leaf c n), rest) -> c : decodeMessage tr rest


