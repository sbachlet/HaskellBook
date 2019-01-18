module DatabaseProcessing where
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving(Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 50
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDate []
  where
    getDate (DbDate x) y  = x : y
    getDate _ y           = y ++ []


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNumber []
  where
    getNumber (DbNumber x) y  = x : y
    getNumber _ y           = y ++ []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr getMostRecent (head $ filterDbDate theDatabase)
  where
    getMostRecent (DbDate x) y
      | y > x         = y
      | otherwise     = x
    getMostRecent _ y = y

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumNumbers 0
  where
    sumNumbers (DbNumber x) y = y + x
    sumNumbers _ y            = y

avgDb :: [DatabaseItem] -> Double
avgDb x =
  fromIntegral
  . div (sumDb x)
  . fromIntegral
  . length
  . filterDbNumber $ x