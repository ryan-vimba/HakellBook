module Sing where

    fstString :: String -> String
    fstString x = x ++ " in the rain"

    sndString :: String -> String
    sndString x = x ++ " over the rainbow"


    
    sing :: (Ord a => a -> a -> Bool) -> String
    sing c = let x = "Singin"
                 y = "Somewhere" in
                if (c x y) then
                           fstString x 
                      else sndString y
    
    
    --sing2 :: (Ord a => a -> a -> (a -> a -> Bool)) -> String
    --sing2 x y c = if (c x y) then
    --                       fstString x 
    --                  else sndString y

 