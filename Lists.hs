module Lists where
    import Data.Char

    myHead :: [a] -> Maybe a
    myHead (x : _) = Just x
    myHead [] = Nothing

    myTail :: [a] -> [a] 
    myTail [] = [] 
    myTail (_ : xs) = xs

    data MyEnum = Foo | Bar | Baz deriving (Show, Enum, Bounded)
    
    eftBool :: Bool -> Bool -> [Bool]
    eftBool a b
        | a < b = a : eftBool (succ a) b
        | a == b = a : []
        | otherwise = []

    -- eftOrd :: Ordering -> Ordering -> [Ordering]

    -- listOfStrings :: String -> [String]
    -- listOfStrings s = takeWhile (\= ' ') s : removeWhiteSpace 
    -- takeWhile (\=' ') (dropWhile (==' ') s)
    myWords :: String -> [String]
    myWords = descend []
        where
            descend :: [String] -> String -> [String]
            -- descend l s = takeWhile (/=' ') (drop dropLen s)
            --     where dropLen = length (dropWhile (==' ') s)
            descend l [] = l
            descend l (x : xs)
                | x == ' ' = descend l (dropWhile (==' ') xs)
                | otherwise =  descend (l ++ [x : takeWhile (/=' ') xs]) (drop takeLen xs)
                    where takeLen = length (takeWhile (/=' ') xs)

    firstSen = "Tyger Tyger, burning bright\n" 
    secondSen = "In the forests of the night\n" 
    thirdSen = "What immortal hand or eye\n" 
    fourthSen = "Could frame thy fearful symmetry?"
    sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

    myLines :: String -> [String]
    myLines = descend []
        where 
            descend :: [String] -> String -> [String]
            descend l "" = l
            descend l s = descend (l ++ [takeWhile (/='\n') s]) (drop (takeLen + 1) s)
                where takeLen = length (takeWhile (/='\n') s)

    myLines' :: String -> [String]
    myLines' "" = []
    myLines' s = line : myLines' rest
        where line = takeWhile (/='\n') s
              rest = drop (length line + 1) s
            

    lengthTakeWhile :: (a -> Bool) -> [a] -> (Int, [a])
    lengthTakeWhile f la = (length tw, tw)
        where tw = takeWhile f la


    myLines'' :: String -> Char -> [String]
    myLines'' "" _ = []
    myLines'' s c = line : (myLines'' rest c)
        where line = takeWhile (/=c) s
              rest = drop (length line + 1) s

    mySqr = [x^2 | x <- [1..5]]
    myCube = [y^3 | y <- [1..5]]

    squareCubeTuples = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

    
    multiplesOfThree = length . filter (\x -> rem x 3 == 0)

    myFilter :: String -> [String]
    myFilter s = filter (\x -> not (elem x ["the", "a", "an"])) (words s)

    -- zip1 :: [a] -> [b] -> [(a, b)]
    -- zip1 [] _ = []
    -- zip1 _ [] = []
    -- zip1 (x : xs) (y : ys) = (x, y) : zip1 xs ys

    -- zip' :: [a] -> [b] -> [(a, b)]
    -- zip' la lb = [(la !! i, lb !! i) | i <- enumFromTo 0 ((min (length la) (length lb)) - 1)]


    zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith' f la lb = [f (la !! i) (lb !! i) | i <- enumFromTo 0 ((min (length la) (length lb)) - 1)]
    
    zip' = zipWith' (,)


    zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith1 _ [] _ = []
    zipWith1 _ _ [] = []
    zipWith1 f (x : xs) (y : ys) = f x y : zipWith1 f xs ys

    zip1  = zipWith1 (,)

    capitalize :: String -> String
    capitalize "" = ""
    capitalize (x:xs) = toUpper x : xs

    capitalizeAll :: String -> String
    capitalizeAll "" = ""
    capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

    capitalizeHead :: String -> Maybe Char
    capitalizeHead "" = Nothing
    capitalizeHead (x:xs) = Just $ toUpper x

    capitalizeAllThenHead :: String -> Maybe Char
    capitalizeAllThenHead s = (capitalizeHead . capitalizeAll) s

    capitalizeAllThenHead' = capitalizeHead . capitalizeAll