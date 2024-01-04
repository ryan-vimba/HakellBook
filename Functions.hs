module Functions where

    addOneIfOdd n = case odd n of
        True -> f n
        False -> n
        where f = \n -> n + 1

    addFive x y = (if x > y then y else x) + 5

    addFive' = \x -> (\y -> (if x > y then y else x) + 5)

    whatever x y = 
        if x > y 
            then y 
            else x


    foo = (addFive 1 10, addFive 10 1)

    mflip f x y = f $ y $ x

    tensDigit :: Integral a => a -> a
    tensDigit x = d
        where xLast = x `div` 10
              d     = xLast `mod` 10

    tensDigit' :: Integral a => a -> a
    tensDigit' x = d
        where xLast = x `divMod` 10
              d     = (fst xLast) `mod` 10      

    hundredsDigits :: Integral a => a -> a
    hundredsDigits x = d
        where xLast = x `divMod` 100
              d     = (fst xLast) `mod` 10  

    expDigits :: Integral a => a -> a -> a
    expDigits x e = d
        where xLast = x `divMod` (10 ^ e)
              d     = (fst xLast) `mod` 10
              
               
    foldBool :: a -> a -> Bool -> a
    foldBool x y z = case z of
        True -> y
        False -> x

    foldBool' :: a -> a -> Bool -> a
    foldBool' x y z
      | z == True = y
      | z == False = x

    g :: (a -> b) -> (a, c) -> (b, c)
    g f (a, c) = (f a, c)