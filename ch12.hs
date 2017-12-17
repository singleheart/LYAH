foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x
