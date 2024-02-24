module Scans where
    
    inf :: [a] -> [a]
    -- inf la = take 500 $ la
    inf = id

    fibs = inf(1 : scanl (+) 1 fibs)
    fibs20 = take 20 $ fibs
    fibsLessThan100 = [x | x <- fibs20, x < 100]
    fibsLessThan100a = filter (<100) fibs20

    factorial = inf(scanl (*) 1 [1..])

    stops  = "pbtdkg"
    vowels = "aeiou"

    stopVowelStop = [s1 : v : [s2] | s1 <- stops, v <- vowels, s2 <- stops]
    stopVowelStopP = [s1 : v : [s2] | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]

    nouns = ["dog", "alien", "pickle", "donut", "chicken"]
    verbs = ["dances", "explodes", "transforms", "tickles", "devours"]

    nounVerbNoun = [n1 ++ " " ++ v ++ " " ++ n2 | n1 <- nouns, v <- verbs, n2 <- nouns]

    avg x = div (sum (map length (words x)))
        (length (words x))

    avg2 x = fromIntegral (sum (map length (words x))) /
        fromIntegral (length (words x))

    myOr :: [Bool] -> Bool
    myOr = foldr (||) False

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f = foldr ((||) . f) False

    myElem :: Eq a => a -> [a] -> Bool
    myElem a = foldr ((||) . (a==)) False
    
    myElem2 :: Eq a => a -> [a] -> Bool
    myElem2 a = any (a==) 

    myReverse :: [a] -> [a]
    myReverse [] = []
    
    myReverse (x:xs) = myReverse xs ++ [x]

    myReverse2 :: [a] -> [a]
    myReverse2 = foldl (flip (:)) []

    myReverse3 :: [a] -> [a]
    myReverse3 = foldr (flip (++).(:[])) []

    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr ((:) . f) []

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = foldr ((++) . pick f) []
        where pick :: (a -> Bool) -> a -> [a]
              pick func a = [a | func a]


    vap :: (a -> Bool) -> a -> (a, Bool)
    vap f a = (a, f a)

    myFilter2 f = foldr ((++) . (\a -> [a | f a])) []

    squish :: [[a]] -> [a]
    squish = foldr (++) []

    squishMap :: (a -> [b]) -> [a] -> [b] 
    squishMap f = foldr ((++) . f) []

    squishAgain :: [[a]] -> [a]
    squishAgain ll = foldr ((++) . squishMap (:[])) [] ll

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
    myMaximumBy f list = foldr (getBigger f) (head list) list
        where getBigger func prev curr = if func prev curr == GT then prev else curr

    compareTemp "coldest" "cold" = LT
    compareTemp "coldest" "hot" = LT
    compareTemp "coldest" "hottest" = LT
    compareTemp "cold" "coldest" = GT
    compareTemp "cold" "hot" = LT
    compareTemp "cold" "hottest" = LT
    -- compareTemp ""

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
    myMinimumBy f list = foldr (getBigger f) (head list) list
        where getBigger func prev curr = if func prev curr == LT then prev else curr



