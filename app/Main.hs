module Main where

import HTTP
import Parse
import UserInput
import Process
import Database

main :: IO ()
main = do

    -- Print the main menu and get the int value of the selected choice
    menuAction <- mainMenu

    -- Gets the title of the film or the name of the actor
    title <- menuInput menuAction

    -- Establishes a database connection
    conn <- initialiseDB

    case menuAction of
        1 -> do
            -- Gets either the film corresponsing to the title or Nothing
            db_film <- searchFilm conn title
            case db_film of
                -- Prints the data from the database if the film has been found.
                Just film -> do
                    print "Film Found in DB"
                    print film
                -- Hits the API to get the data if the the film is not found in the Database.
                Nothing -> do
                    print "Film Not Found in DB. Fetching Film from API"
                    let url = apiString title
                    json <- download url
                    case (parse json) of
                        Left error -> print error
                        Right film -> do
                            -- Prints the film object fetched
                            print $ filmtoObject film
                            -- Stores the film object in the database
                            storeFilms conn [filmtoObject film]
                            -- Stores the actors objects in the database
                            saveActors [filmtoObject film] conn
        2 -> do
            -- Gets either the Actor corresponsing to the title or Nothing
            db_actor <- searchActor conn title
            case db_actor of
                Just actors -> prettyPrint actors
                Nothing -> print "No record found in the DB"
        3 -> do
            -- Gets all films in the Database or Nothing
            db_films <- searchAllFilms conn
            case db_films of
                Just films -> prettyPrint films
                Nothing -> print "No films found in the DB"
        4 -> do
            -- Gets all actors in the Database or Nothing
            db_actors <- searchAllActors conn
            case db_actors of
                Just actors -> prettyPrint actors
                Nothing -> print "No Actors found in the DB"

-- | Pretty prints the list of strings with " | " in between using foldl.
prettyPrint :: [String] -> IO ()
prettyPrint list = print $ foldl (\acc x -> if (null acc) then acc ++ x else acc ++ " | " ++ x) [] list