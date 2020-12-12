{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |This Module works on FilmJSON andRatingJSON received from  the Parse Module and modifies them to extract required data for the database.
module Process (
     FilmObject(..),
     filmtoObject,
     FilmObjects(filmObjects)
) where 

import Util 
import Parse 
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

{- |
 This is to used to take RatingJSON from the Parse file so we can convert the rating value to Double and to calculate
 the average rating per film which will be stored in the database
  -}
data RatingObject = RatingObject{ 
    source' :: String,
    value' :: Double
} deriving (Show,Generic)


-- | The RatingObject function  converts the values of the Ratings from String to Double while multiplying the rating from IMDB by 10 to match the others
ratingObject :: RatingJSON -> RatingObject  
ratingObject (RatingJSON "Internet Movie Database" v) = RatingObject "Internet Movie Database" ((read $ (takeWhile (\c -> c/= '/' && c/= '%') v) :: Double) * 10)
ratingObject (RatingJSON s v) = RatingObject s (read $ (takeWhile (\c -> c/= '/' && c/= '%') v) :: Double)

-- | This is to calculate the average of the ratings received from RatingObject which will be sent to FilmObject and stored as averagerating in the database
calculateAverage :: FilmJSON -> Double 
calculateAverage film = total / n
  where
    rs = ratings film
    fs = [ value' r | r <- map ratingObject rs ]
    total = sum fs
    n = fromIntegral (length fs) :: Double



-- | This is the Format of the Film data that will be sent to the database. 
data FilmObject = FilmObject{
    title' :: String,
    year' :: String,
    runtime' :: String,
    actors' :: String,
    average_rating' :: Double
} deriving (Show,Generic)


-- | This function converts the elements of FilmJSON to approriate Haskell types which are stored as FilmObject
filmtoObject :: FilmJSON -> FilmObject 
filmtoObject v = FilmObject (title v) (year v) (runtime v) (actors v)  ( calculateAverage v) 


-- | This presents the FilmObject as a list of lists to be sent to Database.hs to insertion into the database.
data FilmObjects = FilmObjects { 
   filmObjects :: [FilmObject]
} deriving (Show, Generic)