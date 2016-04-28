{- 
    Validating Credit Card Numbers
-}

-- Exercise 1

divpair :: Integer -> Integer -> (Integer, Integer)
divpair n m = (n `mod` m, n `quot` m)

{- (++) vs (:) u feel?

toDigits :: Integer -> [Integer]
toDigits n | n <= 0 = []
           | otherwise = let (digit, rest) = divpair n 10
                             in (toDigits rest) ++ [digit]
-}

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
              | otherwise = let (digit, rest) = divpair n 10
                                in digit : toDigitsRev rest

toDigits n = reverse $ toDigitsRev n

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]

{- Worked on first try :D -}
doubleEveryOther [] = []
doubleEveryOther x = x' where
                     (x', even) = doubleEveryOther' x
                     doubleEveryOther' :: [Integer] -> ([Integer], Bool)
                     doubleEveryOther' l@(x:[]) = (l, True)
                     doubleEveryOther' (x:xs) = let (xs', even) = doubleEveryOther' xs in
                                                    if even then ((x + x):xs', False)
                                                            else (x:xs', True)

-- Excercise 3

sumDigits :: [Integer] -> Integer
sumDigits l = sum l

-- Exercise 4

toDigitsList :: [Integer] -> [Integer]
toDigitsList [] = []
toDigitsList (x:xs) = toDigits x ++ (toDigitsList xs)

validate :: Integer -> Bool -- Tested this on my credit card, it worked!

validate n = 0 == (sumDigits (toDigitsList (doubleEveryOther (toDigits n)))) `mod` 10

-- Exercise 5 (Towers of Hanoi)

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

-- Exercise 6 (4 Towers of Hanoi)

{-

So, my first thought is that, as the traditional towers of Hanoi problem
is equivalent to traversing an n dimensional hypercube, this problem must
be equivalent to traversing some more convenient shape.

-}
