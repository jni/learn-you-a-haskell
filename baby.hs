doubleMe x = x + x

doubleUs [] = []
doubleUs (x:xs) = doubleMe x : doubleUs xs

upperCase c = c `elem` ['A'..'Z']

lowerCase c = c `elem` ['a'..'z']

myZip :: [a] -> [b] -> [(a, b)]
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x, y) : (myZip xs ys)

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs

bmiDiagnosis :: (RealFloat a) => a -> a -> String
bmiDiagnosis weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / height^2

mytake :: (Num i, Ord i) => i -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs)
    | n <= 0    = []
    | otherwise = x : mytake (n-1) xs

