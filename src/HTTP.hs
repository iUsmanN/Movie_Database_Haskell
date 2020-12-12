-- | module containing functions that download Film data from the API
module HTTP
    ( download,
      apiString
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String

-- | Downloads the Film data from the given URL and returns the response
download :: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- | Creates a URL used to hit the OMDB API
apiString :: String -> String
apiString title = addURLSuffix $ addURLPrefix title

-- | Adds the URL Prefix (Currying Approach)
addURLPrefix :: String -> String
addURLPrefix = (++) "http://www.omdbapi.com/?t="

-- | Adds the URL Prefix (Uncurrying Approach)
addURLSuffix :: String -> String
addURLSuffix prefix = prefix ++ "&apikey=2521df13"