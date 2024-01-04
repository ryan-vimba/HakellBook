module Recursion where

    import Data.List (intersperse)

    fibonacci :: Integral a => a -> a
    fibonacci 0 = 0
    fibonacci 1 = 1
    fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

    type Numerator = Integer
    type Denominator = Integer
    type Quotient = Integer

    dividedBy :: Numerator -> Denominator -> Quotient
    dividedBy = div

    dividedBy2 :: Integral a => a -> a -> (a, a)
    dividedBy2 num denom = descend num denom 0
        where descend n   d count
                    | n < d = (count, n)
                    | otherwise =
                        descend (n - d) d (count + 1)

    cattyConny :: String -> String -> String
    cattyConny x y = x ++ " mrow " ++ y

    flippy = flip cattyConny
    appedCatty = cattyConny "woops"
    frappe = flippy "haha"                      


    {-- 
    dividedBy 15 2
        descend 15 2 0
        descend 13 2 1
        descend 11 2 2
        descend 9 2 3
        descend 7 2 4
        descend 5 2 5
        descend 3 2 6
        descend 1 2 7
    (7, 1)
    --}

    recursiveSum :: (Eq a, Num a) => a -> a
    recursiveSum num = descend num (-num) 0
        where  descend :: (Eq a, Num a) => a -> a -> a -> a
               descend num num_opp sum
                        | num == 0 = sum
                        | num_opp == 0 = (-1)
                        | otherwise =
                            descend (num - 1) (num_opp - 1) (num + sum)

    recursiveMultiply :: (Integral a) => a -> a -> a
    recursiveMultiply a b = descend a b 0
        where descend a b sum
                    | b == 0 = sum 
                    | otherwise = descend a (b - 1) sum + a

    safeDivide :: (Integral a, Fractional b) => a -> a -> Maybe b
    safeDivide n 0 = Nothing
    safeDivide n m = Just(fromIntegral n / fromIntegral m)       

    mccarthy91 :: (Ord a, Integral a) => a -> a
    mccarthy91 num
            | num > 100 = num - 10
            | otherwise = mccarthy91 $ mccarthy91 $ num + 11


    digitToWord :: Int -> String 
    digitToWord n = numberToString !! n
        where numberToString = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        
    digits :: Int -> [Int]
    digits n = descend n []
        where descend n result
                    | n < 10 = n : result
                    | otherwise = descend (div n 10) ((mod n 10) : result)


    concatAll :: [String] -> String
    concatAll [] = ""
    concatAll (x:xs) = x ++ concatAll xs 

    wordNumber :: Int -> String
    wordNumber i = concatAll $ intersperse "-" $ map digitToWord $ digits i            
    
    