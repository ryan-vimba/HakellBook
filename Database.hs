module Database where
    import Data.Time
    data DatabaseItem = DbString String
                        | DbNumber Integer
                        | DbDate   UTCTime
                        deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem] 
    theDatabase = [DbDate    (UTCTime
                             (fromGregorian 1911 5 1)
                             (secondsToDiffTime 34123)), 
                    DbNumber 9001, 
                    DbString "Hello, world!", 
                    DbDate   (UTCTime
                             (fromGregorian 1921 5 1)
                             (secondsToDiffTime 34123))
                  ]
    

    

    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = foldr step [] 
        where step :: DatabaseItem -> [UTCTime] -> [UTCTime] 
              step (DbDate date) times = times ++ [date]
              step _ times = times
    
    filterDbDate2 :: [DatabaseItem] -> [UTCTime]
    filterDbDate2 items = [item | DbDate item <- filter isDbDate items]
        where isDbDate :: DatabaseItem -> Bool
              isDbDate (DbDate _) = True
              isDbDate _ = False

    filterDbDate3 :: [DatabaseItem] -> [UTCTime]
    filterDbDate3 items = [item | DbDate item <- items]

    filterDbDate4 :: [DatabaseItem] -> [UTCTime]
    filterDbDate4 items = items >>= step
        where step (DbDate date) = [date]
              step _ = [] 

    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber items = [number | DbNumber number <- items]

    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = maximum . filterDbDate3

    sumDb :: [DatabaseItem] -> Integer
    sumDb = sum . filterDbNumber

    avgDb :: [DatabaseItem] -> Double
    avgDb items = fromIntegral (sumDb items) / fromIntegral (length items)
    