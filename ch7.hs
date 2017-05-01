import qualified Data.List as L

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack)
