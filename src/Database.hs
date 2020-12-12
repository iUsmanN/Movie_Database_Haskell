{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

-- |Export module containing functions that operate on the database films.sqlite.
module Database
    ( initialiseDB,
      saveActors,
      storeFilms,
      searchActor,
      searchFilm,
      searchAllFilms,
      searchAllActors
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse
import Process
import Util
import Control.Monad
import Data.List
{- |
-- |Method to create the database films.sqlite.
-- It uses connectSqlite3 from Database.HDBC.Sqlite3.
-}
initialiseDB :: IO Connection
initialiseDB =
    do
        conn <- connectSqlite3 "films.sqlite"
        run conn "CREATE TABLE IF NOT EXISTS films (\
            \title TEXT NOT NULL, \
            \year TEXT NOT NULL, \
            \runtime TEXT NOT NULL, \
            \actors TEXT NOT NULL, \
            \averagerating REAL NOT NULL,\
            \PRIMARY KEY(title) \
            \)" []
        commit conn  
        run conn "CREATE TABLE IF NOT EXISTS actors (\
            \name TEXT NOT NULL, \
            \title TEXT NOT NULL, \
            \PRIMARY KEY(name, title) \
            \)" []
        commit conn     
        return conn

-- | Method to process data FilmObject to sql-compatible types.
recordToSqlValues :: FilmObject -> [SqlValue]
recordToSqlValues films = [
        toSql $ title' films, 
        toSql $ year' films,
        toSql $ runtime' films,
        toSql $ actors' films,
        toSql $ average_rating' films
    ]

-- | Method to process fil title and string of actors to sql-compatible types.
recordtoSqlActors :: String -> [String] -> [[SqlValue]]
recordtoSqlActors title actors = [ [toSql actor, toSql title] | actor <- actors ]

-- | Method to insert data into the films table
prepareInsertRecordStmt :: Connection -> IO Statement
prepareInsertRecordStmt conn = prepare conn "INSERT INTO films VALUES (?,?,?,?,?) "

-- | Method to insert data into the actors table
prepareInsertActorsStmt :: Connection -> IO Statement
prepareInsertActorsStmt conn = prepare conn "INSERT INTO actors VALUES (?,?)"

-- | Method to trim blank spaces.
trim :: String -> String
trim = dropWhile (==' ')

-- | Method to save actors data into the actors table.
saveActors :: [FilmObject]
           -> Connection  
           -> IO ()
saveActors films conn = do
    let film = films !! 0
    let film_title = title' film
    let actors = map trim (split ',' (actors' film)) --  spliting actors string into a list of actors
    unique_actors <- filterM (actorNotinDB conn film_title) actors --  filtering actors that are already in DB for that film
    stmt <- prepareInsertActorsStmt conn  --  preparing insert SQL statement
    -- adding new actor/film pairs to DB
    executeMany stmt (recordtoSqlActors film_title unique_actors)
    commit conn

-- | Method to check if the actor exists in the actors table.
actorNotinDB :: Connection -> String -> String -> IO Bool
actorNotinDB conn film_title actor = do
    res <- quickQuery' conn "SELECT title FROM actors WHERE title=(?) AND name=(?)" [toSql film_title, toSql actor]
    return (length res == 0)

-- | Method to check if the film exists in the films table.
filmNotinDB :: Connection -> FilmObject -> IO Bool
filmNotinDB conn film = do
    let title = title' film
    res <- quickQuery' conn "SELECT title FROM films WHERE title=(?)" [toSql title]
    return (length res == 0)

-- | Method to check if duplicate film titles exist in the films table.
compareTitle :: FilmObject ->FilmObject -> Bool
compareTitle f1 f2 = (title' f1) == (title' f2) 

-- | Method to store films in films table.
storeFilms :: Connection 
           -> [FilmObject]
           -> IO ()
storeFilms _ [] = return ()
storeFilms conn xs = do
    let unique = nubBy compareTitle xs
    -- removes duplicates or URLs already in database.
    xs' <- filterM (filmNotinDB conn) unique
    stmt <- prepareInsertRecordStmt conn
    putStrLn "Adding to Database."
    executeMany stmt (map recordToSqlValues xs')
    commit conn

-- | Method to return all films in the film table.
searchAllFilms :: Connection -> IO (Maybe [String])
searchAllFilms conn = do
    res <- quickQuery' conn "SELECT * FROM films " []
    if null res then
        return Nothing
    else do
        let fvalue = map (\xs -> (map fromSql xs :: [String])) res 
        return $ Just $ map head fvalue

-- | Method to return all actors in the actors table.
searchAllActors :: Connection -> IO (Maybe [String])
searchAllActors conn  = do
    res <- quickQuery' conn "SELECT DISTINCT name FROM actors " []
    if null res then
        return Nothing
    else do
        let fvalue = map (\xs -> (map fromSql xs :: [String])) res 
        return $ Just $ map head fvalue

-- | Method to search for a film in the film table.
searchFilm :: Connection  -> String -> IO (Maybe FilmObject)
searchFilm conn film = do
    res <- quickQuery' conn "SELECT * FROM films WHERE title=(?)" [toSql film]
    if null res then
        return Nothing
    else do
        let fvalue = map (\xs -> (map fromSql xs :: [String])) res 
        let film = FilmObject ((!!0) $ head fvalue) ((!!1) $ head fvalue) ((!!2) $ head fvalue) ((!!3) $ head fvalue)  (stringToDouble ((!!4) $ head fvalue)) 
        return $ Just film

-- | Method to search for an actor in the film table and return the films they have performed in.
searchActor :: Connection ->  String -> IO (Maybe [String])
searchActor conn actor = do
    res <- quickQuery' conn "SELECT title FROM actors WHERE name=?" [toSql actor]
    if null res then
        return Nothing
    else do
        let values = map (\xs -> (map fromSql xs :: [String])) res
        return $ Just $ map head values

-- | Method to convert a string to double
stringToDouble :: String -> Double
stringToDouble avgValue = read avgValue :: Double