module SideQuests where

    describeNumber :: Int -> String
    describeNumber n
        | n < 0     = "Negative"
        | n == 0    = "Zero"
        | n < 10    = "Small"
        | n < 100   = "Medium"
        | otherwise = "Large"

    describeNumber2 :: Int -> String
    describeNumber2 n = case n of
        i | i < 0     -> "Negative"
        i | i == 0    -> "Zero"
        i | i < 10    -> "Small"
        i | i < 100   -> "Medium"
        i | otherwise -> "Large"
    
    wierd :: Int -> String
    wierd n = case () of
        i | i == ()     -> "duh!"
        _ -> "Game Over Man"

    wierd2 :: Int -> String
    wierd2 n = case () of
        i | i == ()     -> "duh!"
        otherwise -> "Game Over Man"

{--
    class Num a => Box a where
        contentsOf :: 
        boxOf :: Box
--}        

