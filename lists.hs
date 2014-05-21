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
