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
    