{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module AlgebraicDatatypes where
    data LengthUnit = Meter | Foot
    data Length (a :: LengthUnit) = Length Double deriving Show

    lengthA = Length @Meter 10.0
    lengthB = Length @Foot 10.0

    data Doggies a = 
        Husky a
      | Mastiff a deriving (Eq, Show)
    
    data DogueDeBordeaux doge = DogueDeBordeaux doge

    data Price = Price Integer deriving (Eq, Show)

    data Manufacturer =
        Mini
      | Mazda
      | Tata
        deriving (Eq, Show)

    data Airline =
        PapuAir
      | CatapultsR'Us
      | TakeYourChancesUnited
        deriving (Eq, Show)

    data Size = Size Integer deriving (Eq, Show)

    data Vehicle = Car Manufacturer Price
                 | Plane Airline Size
                 deriving (Eq, Show)

    myCar = Car Mini (Price 14000) 
    urCar = Car Mazda (Price 20000) 
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir (Size 100)
    myPlane = Plane TakeYourChancesUnited (Size 150)

    isCar :: Vehicle -> Bool 
    isCar (Car _ _) = True
    isCar _ = False

    isPlane :: Vehicle -> Bool 
    isPlane (Plane _ _) = True
    isPlane _ = False

    areCars :: [Vehicle] -> [Bool] 
    areCars = map isCar

    getManu :: Vehicle -> Manufacturer 
    getManu (Car manu _) = manu

    data Example = MakeExample deriving Show
    data NewExample a = NewMakeExample Int deriving Show

    class TooMany a where 
      tooMany :: a -> Bool

    instance TooMany Int where 
      tooMany n = n > 42

    newtype Goats = Goats' Int deriving Show
    f = Goats'

    instance TooMany Goats where
      tooMany (Goats' n) = n > 43

    foo (Goats' i) = i

    --unwrapper :: (a -> b) -> a -> b
    --unwrapper @Goats' i = Goats' i
    

    g :: (a -> b) -> a -> b
    g fab = fab

    h = g Goats' 5 

    instance TooMany (Int, String) where
      tooMany (i, s) = i > 42



    

