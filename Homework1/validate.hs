module Validate where 
-- here you should write the the 4 functions of exercises 1-4. The last function is validate
toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
toDigits x = (toDigits (x `div` 10)) ++ [x `mod` 10]   --insert item at the end of the list

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0 = []
toDigitsRev x = (x `mod` 10) : (toDigitsRev (x `div` 10))  --insert item at the beginning of the list

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) 
                 | not (even (length xs)) = (x * 2) : doubleEveryOther xs
                 | otherwise = x : doubleEveryOther xs

-- doubleEveryOther (x:y:xs) = x : 2*y : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + (sumDigits xs) 

validate :: Integer -> Bool
validate x | sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0 = True
           | otherwise = False 