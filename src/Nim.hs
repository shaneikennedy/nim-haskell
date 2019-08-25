module Nim
    ( playNim
    ) where

whosTurn :: Bool -> String
whosTurn True = "User's turn!"
whosTurn _ = "Computer's turn!"

getHumanInput :: Int -> IO String
getHumanInput count = do
  putStrLn $
    "Enter a number between 1 and " ++ show (ceiling $ fromIntegral count / 2)
  getLine

reportScore :: Int -> IO ()
reportScore num = putStrLn $ "The count is now " ++ show num

playHumanTurn :: Int -> IO ()
playHumanTurn total = do
  putStrLn $ whosTurn True
  num <- getHumanInput total
  reportScore $ total - read num
  playNim (total - read num) False

playComputerTurn :: Int -> IO ()
playComputerTurn total = do
  putStrLn $ whosTurn False
  reportScore (total - 1)
  playNim (total - 1) True 
  

playNim :: Int -> Bool -> IO ()
playNim total userTurn
  | total <= 0 = putStrLn (whosTurn userTurn ++ " wins!")
  | userTurn = playHumanTurn total
  | otherwise = playComputerTurn total
 
