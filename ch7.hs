import qualified Data.List as L
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack)

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

encode' :: Int -> String -> String
encode' shift msg = map (chr . (+ shift) . ord) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
