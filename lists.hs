import qualified Data.List as L

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = L.concat . (map f)

concatMap'' :: (a -> [b]) -> [a] -> [b]
concatMap'' _ [] = []
concatMap'' f (x:xs) = f x ++ concatMap'' f xs

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p = foldr (place p) ([], [])
             where
                place :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
                place p x (xs, ys)
                    | p x       = (x:xs, ys)
                    | otherwise = (xs, x:ys)

-- doesn't work
-- squareFirstOver10 :: (Num a) => [a] -> Maybe a
-- squareFirstOver10 xs = (L.find (>10) xs)^2

squareFirstOver10 :: (Num a, Ord a) => [a] -> Maybe a
squareFirstOver10 xs = square $ L.find (>10) xs
                       where
                           square :: (Num a) => Maybe a -> Maybe a
                           square Nothing  = Nothing
                           square (Just x) = Just (x^2)

groupBy' :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy' key = L.groupBy (\x y -> key x == key y)
