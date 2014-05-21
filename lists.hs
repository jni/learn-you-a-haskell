import qualified Data.List as L

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = L.concat . (map f)

concatMap'' :: (a -> [b]) -> [a] -> [b]
concatMap'' _ [] = []
concatMap'' f (x:xs) = f x ++ concatMap'' f xs
