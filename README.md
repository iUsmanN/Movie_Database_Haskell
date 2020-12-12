# Movie Database (Haskell)


## Overview
This project downloads and parses JSON data from the OMBD Films API. The Menu asks user for an input whether to search a Film or an Actor. As the API only provides film data, the actors can only be fetched from the local database. Once a Film name is entered, the program first searches for the film in the local database. If found, it simply displays the film details. Otherwise, the API call is made to fetch and display the film.
This data is parsed to calculate average ratings and extract actors to update the Films and Actors database. In each film object, ‘Title’, ‘Year’, ‘Runtime’, ‘Actor’ and ‘Average Rating’ fields are stored in the database. The database consists of two tables: Films and Actors. Both these tables have a many-to-many relationship between them and are based on SQLite.

The four main functionalities provided in the project are:
1 – Search Movies by Title
2 – Search Actors by Name
3 – Get All Films in the Local Database 
4 – Get All Actors in the Local Database

In order to compile and build the project, simply run the command stack build in the project directory followed by the command stack exec fpproject-exe.


## User Input
The UserInput module handles the Main Menu of the project. It contains the functions to prompt and take the inputs for the main menu as well as any sub menu required. The mainMenu function uses a >>= Monadic programming binding syntax to handle any exceptions and return the menu choice made by the user. While the menuInput function gets any further details if required.


## Downloading Data
The HTTP module contains the functions used to generate the API String and download against the URL (String). The API endpoint used to download the data is http://www.omdbapi.com/?t=[title] and the API Key used is 2521df13. HTTP- Conduit and Byte String dependencies are used for this purpose. The Aeson dependency is later used to parse the data in the Parse.hs file and make FilmJSON and RatingJSON objects. In order to handle the Capital cases in the JSON field labels, custom deriving method is defined to creating each of the Haskell objects.
  

## Parsing Data
Parse file, another file named the ‘Process file’ obtains filmJSON and RatingJSON, performs on the function ‘RatingObject’ that converts the values of the film's rating from filmJSON and convert them to Double. Although, the final format of the Rating sources and their respective values were initially to be stored as RatingObject, due to type incompatibility with SQLite, the rating values that is going to be used by the calculateAverage function to calculate the average rating from the three sources which will be stored in FilmObject as average rating.
Originally , the FilmtoObject will be used to perform necessary conversion on the actors by splitting the string to obtain the names of individual actors starring in the file, however it has been moved to the Database file, besides the final format of the FilmObject will be deliver over to the Database file where the implementation has been done through the Haskell compatible software ‘Visual Studio Code’ for Windows and stack commands were run in the in-built command terminal.


## Database
In order to complete the database functionality of the project, we must design the database in accordance with the data that is downloaded from the API. As such the data to be downloaded was for films, the raw JSON data consisted of many fields such as title, year, rated, released, actors, rating, etc. Hence, we decided on the following fields to be stored in the database: title, year, runtime, actors and rating.
The database design is as follows, a films table containing the fields: title, year, runtime, average rating. Additionally, we will have an actor’s table containing actor name and film title. As we are using SQLite as our database of choice, we can only have 5 data types and as such all fields in actors and films tables will be TEXT, with the exception of average Rating as a REAL. On the assumption that all film titles are unique, we designate film title as the primary key for films and actor name will also be a primary key in the actors table. This models a one-to-one cardinality between entries in films and actors tables.

Hence, we write Haskell functions to create and manage the database. Such functions include creating tables if they do not exist; processing Haskell data types to and from SQL; data insertion statements; and check functions if there are already existing entries or are not present in the database. Other methods were created with the intention to allow the user to query the database, such as searching for films an actor has performed in or retrieving all films in the database.

The implementation of the code to create the database was done with Haskell on Sublime Text 3, where Haskell stack in command prompt was used to run the project and DB Browser SQLite was used to verify the database entries.
