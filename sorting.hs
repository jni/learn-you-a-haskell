quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort [y | y <- xs, y < x] ++
                    [x] ++
                    quicksort [y | y <- xs, y >= x])

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
               where (left, right) = splitAt (length xs `div` 2) xs
                     merge xs [] = xs
                     merge [] ys = ys
                     merge (x:xs) (y:ys)
                        | x <= y = x : merge xs (y:ys)
                        | x >  y = y : merge (x:xs) ys
