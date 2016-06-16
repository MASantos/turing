module HTypes ( Tree(Leaf,Node),
               Bit(L,R),
               HCode,
               Table, fromStringToBits ) where

data Bit = L | R deriving (Eq,Show)

type HCode = [Bit] 

type Table = [(Char,HCode)]

data Tree = Leaf Char Int | Node Int Tree Tree

fromStringToBits :: String -> [Bit]
fromStringToBits  [] = []
fromStringToBits (c:st) = s: (fromStringToBits st) where
                       s | c:[] == (show L) = L
                         | c:[] == (show R) = R
                         | otherwise  = error "Character not a bit"
