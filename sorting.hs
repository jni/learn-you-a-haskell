import qualified Data.List as L

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:other) = left ++ [pivot] ++ right
                          where
                              left  = quicksort (filter (<= pivot) other)
                              right = quicksort (filter (>  pivot) other)

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

insort :: (Ord a) => [a] -> [a]
insort [] = []
insort (x:xs) = L.insert x (insort xs)
