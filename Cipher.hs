module Cipher where
    import Data.Char 

    caesar :: String -> Int -> String
    caesar "" _ = ""
    caesar (x : xs) shiftAmount = shiftedChar : caesar xs shiftAmount
        where 
            charIndexInAlphabet = ord x - ord 'a'
            shiftedIndex = charIndexInAlphabet + shiftAmount
            wrappedIndex = shiftedIndex `mod` 26
            asciiAdjustedIndex = wrappedIndex + ord 'a'
            shiftedChar = chr asciiAdjustedIndex

    unCaesar :: String -> Int -> String
    unCaesar "" _ = ""
    unCaesar (x : xs) unshiftAmount = unshiftedChar : unCaesar xs unshiftAmount
        where
            charIndexInAlphabet = ord x - ord 'a'
            unshiftedIndex = charIndexInAlphabet - unshiftAmount
            wrappedIndex = unshiftedIndex `mod` 26
            asciiAdjustedIndex = wrappedIndex + ord 'a'
            unshiftedChar = chr asciiAdjustedIndex

    roundTripCaesar :: Int -> String -> String
    roundTripCaesar = \n -> flip (unCaesar . flip caesar n) n

    -- roundTripCaesar' :: Int -> String -> String
    -- roundTripCaesar' n = unCaesar n . flip caesar n

    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (b : xb) = b || myOr xb

    myOr2 :: [Bool] -> Bool
    myOr2 [] = False
    myOr2 (True : _) = True
    myOr2 (False : xb) = myOr2 xb

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f list = or [f e | e <- list]

    myAny' :: (a -> Bool) -> [a] -> Bool
    myAny' f list = elem True $ map f list

    nums = [1, 2, 3, 4, 5]
    f x = x > 5

    myElem :: Eq a => a -> [a] -> Bool
    myElem e [] = False
    myElem e (x : xs) = (e == x) || myElem e xs

    myElem' :: Eq a => a -> [a] -> Bool
    myElem' e list = any (e ==) list
    
    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x : xs) = myReverse xs ++ [x]

    squish :: [[a]] -> [a]
    squish [] = []
    squish [[]] = []
    squish (x : xs) = x ++ squish xs

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f list = squish $ map f list

    f1 :: a -> [a]
    f1 x = [x, x, x]

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id