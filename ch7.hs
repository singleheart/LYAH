import qualified Data.List as L

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub
