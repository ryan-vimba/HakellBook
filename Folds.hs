module Folds where
    -- foldr (*) 1 [1..5] -->  (1 * (2 * (3 * (4 * (5 * 1)))))
    -- foldl (*) 1 [1..5] -->  (((((1 * 1) * 2) * 3) * 4) * 5)

    -- foldr (++) ["woot", "WOOT", "woot"]
    example1 = foldr (++) "" ["woot", "WOOT", "woot"]

    -- foldr max [] "fear is the little death"
    example2 = foldr max '\0' "fear is the little death"

    -- foldr and True [False, True]
    example3 = foldr (&&) True [False, True]

    -- foldr (||) True [False, True]
    example4 = foldr (||) True [False, True]
    -- (False || (True || True))

    foldrOr :: [Bool] -> Bool
    -- foldrOr la = foldr (||) True la
    foldrOr la = id True

    -- foldl ((++) . show) "" [1..5]
    example5 = foldr ((++) . show) "" [1..5]

    -- foldr const 'a' [1..5]
    example6 = foldr (flip const) 'a' [1..5]
    -- (1 @ (2 @ (3 @ (4 @ (5 @ 'a')))))

    -- foldr const 0 "tacos"
    example7 = foldr (flip const) 0 "tacos"

    -- foldl (flip const) 0 "burritos"
    example8 = foldl const 0 "burritos"

    -- foldl (flip const) 'z' [1..5]
    example9 = foldl const 'z' [1..5]
    
    

