-- {-# LANGUAGE NoMonomorphismRestriction #-}

module Chap5Exercises where

co1 :: (b -> c) -> (a -> b) -> (a -> c)
co2 :: (b -> c) -> (a -> b) -> a -> c


co1 bc ab = \a -> bc (ab a)
co2 bc ab a = bc (ab a)


a :: (a -> c) -> a -> a
a _ x = x 

a' :: (a -> b) -> a -> b
a' ab a = ab a


f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g $ f(i)

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w $ q a 

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w

munge xy ywz x = fst (ywz (xy x))

munge' :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w

munge' xy ywz x = myfst $ ywz $ xy x
                  where 
                    myfst :: (a, b) -> a
                    myfst (x, _) = x

myConst :: a -> String
myConst _ = "foo"


data Thing = Quantity Integer | Text String
data NumberThing = Whatever 

list = [Quantity 1, Text "Awesome", Quantity 5]

x = 5