import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFucntion [] . words
  where foldingFucntion (x:y:ys) "*" = (x * y):ys
        foldingFucntion (x:y:ys) "+" = (x + y):ys
        foldingFucntion (x:y:ys) "-" = (y - x):ys
        foldingFucntion (x:y:ys) "/" = (y / x):ys
        foldingFucntion (x:y:ys) "^" = (y ** x):ys
        foldingFucntion (x:xs) "ln" = log x:xs
        foldingFucntion xs "sum" = [sum xs]
        foldingFucntion xs numberString = read numberString:xs
