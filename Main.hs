module Main where

    main :: IO ()
    main = putStrLn "test"

    rev :: String -> String
    rev "" = ""
    rev s = (last s) : (rev (init s))

    rev2 :: String -> String
    rev2 s = if s == "" 
            then "" 
            else (last s) : (rev2 (init s))

    rev3 :: String -> String
    rev3 "" = ""
    rev3 s = (rev3 (tail s)) ++ (head s) : ""         

    offset :: Char -> Integer -> Integer -> (String, Integer)
    offset _ a 0 = ("Sum", a) 
    offset _ 0 b = ("Difference", b)   -- Actually this is wrong
    offset '+' a b = let sum x y = x + y
                    in ("Sum", sum a b)
    offset '-' a b = ("Difference", diff a b)
                    where diff a b = a - b
    offset invalidOp _ _ = if (invalidOp == '*') || (invalidOp == '/') 
                            then ("Mult or Division not supported", 0) 
                            else ("Invalid Operation", 0) 


    getX :: (Float, Float, Float) -> Float
    getX (x, _, _) = x

    (?..) :: (Float, Float, Float) -> Float
    (?..) (x, _, _) = x


    getY :: (Float, Float, Float) -> Float
    getY (_, y, _) = y

    getZ :: (Float, Float, Float) -> Float
    getZ (_, _, z) = z

    (<.>) :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
    (<.>) d p = (getX p + getX d, 
                    getY p + getY d,
                    getZ p + getZ d)

    (/||) :: Bool -> Bool -> Bool
    (/||) True True = False
    (/||) False False = False
    (/||) _ _ = True

    (/|||) :: Bool -> Bool -> Bool
    (/|||) l r = case (l, r) of (True, True) -> False
                                (False, False) -> False
                                _ -> True




    xor :: Bool -> Bool -> Bool
    xor a b = a /= b

    xor2 :: Bool -> Bool -> Bool
    xor2 a b = if a then
                if b then False
                else True
            else
                if b then True
                else False

    data ArithOp = Add | Sub | Mult | Div
    (.+.) = Add
    (.-.) = Sub
    (.*.) = Mult
    (./.) = Div


    compute :: ArithOp -> Float -> Float -> Float
    compute op l r =
        case op of 
                Add -> doAdd l r
                Sub -> doSub l r
                Mult -> doMult l r
                Div -> l / r
        where 
            doAdd a b = a + b
            doSub a b = a - b
            doMult a b = a * b     


    check :: Float -> Bool
    check v =
        if v == 1 then True 
        else if v == 2 then False
        else if v == 3 then True
        else False


    tail_ :: String -> String
    tail_ (h : t) = t

    extract :: String -> Int
    extract s =
        case s of 
            (h : t) | (length t) > 5 -> 1000
            (h : t) -> (length t)
            _       -> -1

    isPalindrome :: (Eq a) => [a] -> Bool
    isPalindrome a = a == (reverse a) 

    myAbs :: Integer -> Integer
    myAbs v =
        if v >= 0 then v
        else -v

    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f ab cd = ((snd ab, snd cd), (fst ab, fst cd))

    x = (+)

    ff :: String -> Int
    ff xs = w `x` 1
        where w = length xs

    ident1 :: a -> a
    ident1 a = a

    ident2 :: a -> a
    ident2 = \a -> a

    myMap :: (a -> b) -> [a] -> [b]
    myMap f la = case la of
                [] -> []
                (a:t) -> (f a) : (myMap f t)

    -- Don't currying: for cartesian points

    nudge :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
    nudge (x, y, z) = (x + 1, y + 1, z + 1)

    -- Do use currying: for dimensions (e.g. LxWxH)


    data DimLength = DimLength Integer
    data DimWidth = DimWidth Integer
    data DimHeight = DimHeight Integer  

    volume :: DimLength -> DimHeight -> DimWidth -> Integer
    volume (DimLength l) (DimHeight h) (DimWidth w) = l * h * w 

    curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
    curry3 fabc xa xb xc = fabc (xa, xb, xc)

    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 fabc (xa, xb, xc) = fabc xa xb xc

    divide :: (Fractional a) => a -> a -> a
    divide x y = x / y

    faaa :: a -> a -> a -> a
    faaa z x y = z
    c = 'c'

    gg = faaa c





