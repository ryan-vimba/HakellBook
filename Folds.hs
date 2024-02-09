module Folds where
    {--
    
        Relationship between foldr and foldl:
        foldl (flip f) z (reverse xs) = foldr f z xs
    
    --}
    
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

    -- foldr const 0 xs
    -- = foldr const 0 ([1,2,3,4,5] ++ undefined)
    -- = foldr const 0 [1,2,3,4,5] ++ undefined
    -- = const 1 (foldr const 0 [2,3,4,5] ++ undefined )
    -- = const 1 (const 2 (foldr const 0 [3,4,5] ++ undefined))
    -- = const 1 (const 2 (const 3 (foldr const 0 [4,5] ++ undefined)))
    -- = const 1 (const 2 (const 3 (const 4(foldr const 0 [5] ++ undefined))))
    -- = const 1 (const 2 (const 3 (const 4(const 5 (foldr const 0 [] ++ undefined)))))
    -- = const 1 (const 2 (const 3 (const 4(const 5 (foldr const 0 [] ++ undefined)))))

    -- = 1
    
    --- = foldr const 0 


    -- example11 = foldr (&&) False ([True, undefined])
    -- foldr (&&) False ([True, undefined])
    -- True && (foldr (&&) False ([undefined])
    -- True && (undefined && (foldr (&&) False []))
    
    example12 = foldr (&&) True [False, undefined]
    example13 = foldr (&&) True ([False] ++ undefined)

    -- xs = [1..5]  
    -- foldl const 0 xs
    -- foldl const 0 [1,2,3,4,5] ++ undefined
    -- foldl const (const 0 1) [2,3,4,5] ++ undefined
    -- foldl const 0 [2,3,4,5] ++ undefined
    -- foldl const (const 0 2) [3,4,5] ++ undefined
    -- foldl const 0 [3,4,5] ++ undefined

    -- ...
    -- foldl const 0 [] ++ undefined

    example14 = foldl (&&) False [False, True, False, undefined]
    -- foldl (&&) (False && False) [True, False, undefined]
    -- foldl (&&) ((False && False) && True) [False, undefined]
    -- foldl (&&) (((False && False) && True) && False) [undefined]
    -- foldl (&&) ((((False && False) && True) && False) && undefined) []
    -- (((False && False) && True) && False) && undefined
    -- ((False && True) && False) && undefined
    -- (False && False) && undefined
    -- False && undefined
    -- False
