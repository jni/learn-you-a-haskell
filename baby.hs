doubleMe x = x + x

doubleUs [] = []
doubleUs (x:xs) = doubleMe x : doubleUs xs
