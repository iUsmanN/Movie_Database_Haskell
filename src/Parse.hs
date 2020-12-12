{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |The Parse Module contains functions that take the downloaded JSON data and convert to matching haskell Data types.
module Parse (
    parse,
    RatingJSON(..),
    FilmJSON(..),
) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- | Data Type to store the Ratings Object in the JSON response
data RatingJSON = RatingJSON {
    source :: String,
    value :: String
} deriving (Show, Generic)

-- | Handles the Capital Casing in the JSON response
$(deriveFromJSON defaultOptions {
    fieldLabelModifier = let f "source" = "Source"
                             f "value" = "Value"
                         in f
} ''RatingJSON)

-- | Data Type to store the Film Object in the JSON response
data FilmJSON = FilmJSON {
    title :: String,
    year :: String,
    runtime :: String,
    actors :: String,
    ratings :: [RatingJSON]
} deriving (Show, Generic)

-- | Handles the Capital Casing in the JSON response
$(deriveFromJSON defaultOptions {
    fieldLabelModifier = let f "title" = "Title"
                             f "year" = "Year"
                             f "runtime" = "Runtime"
                             f "actors" = "Actors"
                             f "ratings" = "Ratings"
                         in f
} ''FilmJSON)

instance ToJSON RatingJSON
instance ToJSON FilmJSON

-- | Parses the JSON response to return the FilmJSON object
parse :: L8.ByteString -> Either String FilmJSON
parse json = eitherDecode json :: Either String FilmJSON