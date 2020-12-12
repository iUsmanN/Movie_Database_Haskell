
-- | This Module provides a user interface and recevies input from the user
module UserInput (
    menuInput,
    mainMenu
) where

import System.IO
import Control.Exception

-- | Parses the Int value to return an error if an Int value was not entered.
parseInt :: Exception e => String -> IO (Either e Int)
parseInt s = try $ evaluate (read s :: Int)

-- | Prompts an input from the User.
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- | Prints the Main Menu and returns the Integer corresponding to the menu item.
mainMenu :: IO Int
mainMenu = do
  putStrLn "~ Welcome ~"
  putStrLn "~ Please select what you want to search ~"
  putStrLn "~ 1 - Movies"
  putStrLn "~ 2 - Actors (DB Only)"
  putStrLn "~ 3 - Get All Films (DB Only)"
  putStrLn "~ 4 - Get All Actors (DB Only)"
  
  (prompt "Input: ") >>= (\a -> (parseInt a :: IO (Either SomeException Int)) >>= (\b -> 
    case b of
     Left e -> return 0
     Right n -> return n
    ))

-- | Gets the title of the film or the name of the actor where required.
menuInput :: Int -> IO String
menuInput input
    | input == 1 = do
      input <- prompt "Enter Movie Name: "
      return input
    | input == 2 = do
      input <- prompt "Enter Actor Name: "
      return input
    | input == 3 = do
      print "Getting All Films"
      return ""
    | input == 4 = do
      print "Getting All Actors"
      return ""  
    | otherwise = do
      putStrLn "Invalid Entry"
      return ""