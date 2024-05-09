module Chap12Exercises where

    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe x = Just x

    stringify :: String -> Maybe String -> String
    stringify _ (Just s) = s
    stringify e Nothing = e

    -- [Just "I", Just "am", Nothing, Just "perfect", Just "test"]
    replaceThe :: String -> String
    replaceThe text = concatMap ((++ " ") . stringify "a" . notThe) (words text)


    startsWithVowel :: String -> Bool
    startsWithVowel ('a' : _) = True
    startsWithVowel ('e' : _) = True
    startsWithVowel ('i' : _) = True
    startsWithVowel ('o' : _) = True
    startsWithVowel ('u' : _) = True
    startsWithVowel _ = False
    
    isVowel :: Char -> Bool
    isVowel ch = startsWithVowel (ch : "") 
    
    intify :: String -> String -> Int
    intify "the" word = if(startsWithVowel word) then 1 else 0
    intify _ _ = 0

    countTheBeforeVowelInList :: [String] -> Int
    countTheBeforeVowelInList [] = 0
    countTheBeforeVowelInList [_] = 0
    countTheBeforeVowelInList (x : y : xs) = intify x y + countTheBeforeVowelInList (y : xs)

    countTheBeforeVowel :: String -> Int
    countTheBeforeVowel text = countTheBeforeVowelInList (words text)

    countVowels :: String -> Int
    countVowels text = length $ filter isVowel text

    newtype Word' = Word' String deriving (Eq, Show)

    vowels = "aeiou"

    mkWord :: String -> Maybe Word'
    mkWord s = let 
                   countConsonants :: String -> Int
                   countConsonants = foldr (\ch n -> (if notElem ch vowels then n + 1 else n)) 0
                   countVowels :: String -> Int
                   countVowels = foldr (\ch n -> (if elem ch vowels then n + 1 else n)) 0 
                 in
                   case countVowels s > countConsonants s of
                        True -> Nothing
                        False -> Just $ Word' s
    
    data Nat = Zero | Succ Nat deriving (Eq, Show)

    one = Succ Zero
    two = Succ $ Succ Zero
    three = Succ $ Succ $ Succ Zero

    three' = Succ two

    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ n) = 1 + natToInteger n

    integerToNat :: Integer -> Maybe Nat
    integerToNat n 
        | n < 0 = Nothing 
        | n == 0 = Just Zero 
        | otherwise = integerToNat (n - 1) >>= (Just . Succ)

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing = False

    isNothing :: Maybe a -> Bool
    isNothing = not . isJust

    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee b _ Nothing = b
    mayybee _ f (Just a) = f a

    fromMaybe :: a -> Maybe a -> a
    fromMaybe a Nothing = a
    fromMaybe _ (Just a) = a

    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just a) = [a]

    catMaybes :: [Maybe a] -> [a]
    catMaybes = concatMap maybeToList

    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe l 
        | any isNothing l = Nothing
        | otherwise = Just $ catMaybes l


    

    