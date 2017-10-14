import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFucntion [] . words
  where foldingFucntion (x:y:ys) "*" = (x * y):ys
        foldingFucntion (x:y:ys) "+" = (x + y):ys
        foldingFucntion (x:y:ys) "-" = (y - x):ys
        foldingFucntion xs numberString = read numberString:xs
