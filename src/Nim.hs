module Nim
  ( playNim
  ) where

import System.Random

whosTurn :: Bool -> String
whosTurn True = "User"
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
  putStrLn $ whosTurn True ++ " turn"
  num <- getHumanInput total
  reportScore $ total - read num
  playNim (total - read num) False

playComputerTurn :: Int -> IO ()
playComputerTurn total = do
  putStrLn $ whosTurn False ++ " turn"
  computerChoice <- getRandomNumber (ceiling $ fromIntegral total / 2)
  putStrLn $ "Computer chose: " ++ show computerChoice
  reportScore (total - computerChoice)
  playNim (total - computerChoice) True

playNim :: Int -> Bool -> IO ()
playNim total userTurn
  | total <= 0 = putStrLn (whosTurn userTurn ++ " wins!")
  | userTurn = playHumanTurn total
  | otherwise = playComputerTurn total
