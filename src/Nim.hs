module Nim
  ( User(Human, Computer)
  , playNim
  ) where

import System.Random

data User
  = Human
  | Computer
  deriving (Eq)

whosTurn :: User -> String
whosTurn Human = "User"
whosTurn _ = "Computer"

getRandomNumber :: Int -> IO Int
getRandomNumber upperRange = randomRIO (1, upperRange :: Int)

getHumanInput :: Int -> IO String
getHumanInput count = do
  putStrLn $
    "Enter a number between 1 and " ++
    show (ceiling (fromIntegral count / 2 :: Double) :: Int)
  getLine

reportScore :: Int -> IO ()
reportScore num = putStrLn $ "The count is now " ++ show num

playHumanTurn :: Int -> IO ()
playHumanTurn total = do
  putStrLn $ whosTurn Human ++ " turn"
  num <- getHumanInput total
  reportScore $ total - read num
  playNim (total - read num) Computer

playComputerTurn :: Int -> IO ()
playComputerTurn total = do
  putStrLn $ whosTurn Computer ++ " turn"
  computerChoice <- getRandomNumber (ceiling $ fromIntegral total / 2)
  putStrLn $ "Computer chose: " ++ show computerChoice
  reportScore (total - computerChoice)
  playNim (total - computerChoice) Human

playNim :: Int -> User -> IO ()
playNim total userTurn
  | total <= 0 = putStrLn (whosTurn userTurn ++ " wins!")
  | userTurn == Human = playHumanTurn total
  | otherwise = playComputerTurn total
