{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module AlgebraicDatatypes where
    
    import Data.Int
    import Distribution.Simple (extensionsToFlags)
    
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
    getManu (Plane _ _) = undefined

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


    data NumberOrBool =
         Numba Int8
        | BoolyBool Bool deriving (Eq, Show)
    
    data Person0 = 
      MkPerson String Int 
        deriving (Eq, Show)

    data Person =
      Person { name :: String
              , age :: Int } 
              deriving (Eq, Show)

    jm = Person "julie" 108 
    ca = Person "chris" 16

    data Fiction0 = Fiction deriving Show
    data Nonfiction0 = Nonfiction deriving Show
    data BookType = FictionBook Fiction0
                  | NonfictionBook Nonfiction0
                  deriving Show

    type AuthorName = String
    -- data Author = Author (AuthorName, BookType)

    data Author = Fiction0 AuthorName
                | Nonfiction0 AuthorName
                deriving (Eq, Show)

    data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

    type Gardener = String

    data Garden = GardeniaGarden Gardener
                | DaisyGarden Gardener
                | RoseGarden Gardener
                | LilacGarden Gardener
                deriving Show

    data GuessWhat =
      ChickenButt deriving (Eq, Show)
    data Id a =
      MkId a deriving (Eq, Show)

    data Product a b =
      Product a b deriving (Eq, Show)

    data Sum a b = 
        First a
      | Second b
      deriving (Eq, Show)

    data RecordProduct a b = 
      RecordProduct { pfirst :: a
                    , psecond :: b } 
                    deriving (Eq, Show)

    data OperatingSystem =
       GnuPlusLinux
      | OpenBSDPlusNevermindJustBSDStill | Mac
      | Windows
      deriving (Eq, Show)

    data ProgLang =
          Haskell
        | Agda
        | Idris
        | PureScript deriving (Eq, Show)

    data Programmer =
      Programmer { os :: OperatingSystem
                  , lang :: ProgLang } 
      deriving (Eq, Show)

    allOperatingSystems :: [OperatingSystem] 
    allOperatingSystems =
      [ GnuPlusLinux
      , OpenBSDPlusNevermindJustBSDStill , Mac
      , Windows
      ]

    allLanguages :: [ProgLang] 
    allLanguages = [Haskell, Agda, Idris, PureScript]

    allProgrammers :: [Programmer]
    allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

    -- partialAf = Programmer {os = GnuPlusLinux}
    -- This will not compile due to lang. extension

    -- partialFunction :: Bool -> Bool
    -- partialFunction True = True

    data Quantum = Yes | No | Both deriving (Eq, Show)

    convert0 :: Quantum -> Bool
    convert0 Yes = True
    convert0 No = True
    convert0 Both = True

    convert1 :: Quantum -> Bool
    convert1 Yes = True
    convert1 No = True
    convert1 Both = False

    convert2 :: Quantum -> Bool
    convert2 Yes = True
    convert2 No = False
    convert2 Both = True
    
    convert3 :: Quantum -> Bool
    convert3 Yes = False
    convert3 No = True
    convert3 Both = True

    convert4 :: Quantum -> Bool
    convert4 Yes = False
    convert4 No = False
    convert4 Both = True

    convert5 :: Quantum -> Bool
    convert5 Yes = True
    convert5 No = False
    convert5 Both = False

    convert6 :: Quantum -> Bool
    convert6 Yes = False
    convert6 No = True
    convert6 Both = True

    convert7 :: Quantum -> Bool
    convert7 Yes = False
    convert7 No = False
    convert7 Both = False
    
    type Converter = Quantum -> Maybe Bool

    -- convertSingle :: Quantum -> Bool -> Maybe Bool
    -- convertSingle q' b  = (\q -> if(q == q') then Just(b) else Nothing)    
    
    -- convertN :: [Converter]
    -- [convertSingle(q', b) | q' <- [Yes, No, Both], b <- [True, False]]

    data Quad = One | Two | Three | Four deriving (Eq, Show)

    eQuad :: Either Quad Quad
    eQuad = Left One  -- Left One, Left Two, Left Three, Left Four, Right One, Right Two, Right Three, Right Four

    data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

    mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    mapTree _ Leaf = Leaf
    mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

    testTree' :: BinaryTree Integer
    testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

    mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

    mapOkay = if mapTree (+1) testTree' == mapExpected
              then print "yup okay!"
              else error "test failed!"

    preorder :: BinaryTree a -> [a]
    preorder Leaf = []
    preorder (Node left a right) = [a] ++ preorder left ++ preorder right

    inorder :: BinaryTree a -> [a]
    inorder Leaf = []
    inorder (Node left a right) = inorder left ++ [a] ++ inorder right

    postorder :: BinaryTree a -> [a]
    postorder Leaf = []
    postorder (Node left a right) = postorder left ++ postorder right ++ [a]

    testTree :: BinaryTree Integer
    testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

    testPreorder :: IO ()
    testPreorder = if preorder testTree == [2, 1, 3]
                    then putStrLn "Preorder fine!"
                    else putStrLn "Bad news bears."

    testInorder :: IO ()
    testInorder = if inorder testTree == [1, 2, 3]
                    then putStrLn "Inorder fine!"
                    else putStrLn "Bad news bears."

    testPostorder :: IO ()
    testPostorder = if postorder testTree == [1, 3, 2]
                    then putStrLn "Postorder fine!"
                    else putStrLn "postorder failed check"

    main :: IO ()
    main = do
      testPreorder
      testInorder
      testPostorder

    foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldTree _ b Leaf = b
    foldTree f b (Node left a right) = foldTree f (f a (foldTree f b left)) right

    vigenereCipher :: String -> String -> String
    vigenereCipher "" _ = ""
    vigenereCipher (x : xs) keyword = undefined
      where extendedKeyword = matchKeywordLength keyword (length xs + 1)
    
    -- getShiftAmount :: String -> Int -> Int
    -- getShiftAmount keyword index = 

    matchKeywordLength :: String -> Int -> String
    matchKeywordLength keyword inputLength
      | length keyword < inputLength = matchKeywordLength (keyword ++ keyword) inputLength
      | otherwise = take inputLength keyword




    



