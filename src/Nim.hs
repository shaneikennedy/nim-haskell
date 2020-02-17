module Nim
  ( User(Human, Computer)
  , playNim
  ) where

data User
  = Human
  | Computer
  deriving (Eq)

whoseTurn :: User -> String
whoseTurn Human = "User"
whoseTurn _ = "Computer"

getSmartGuess :: Int -> Int
getSmartGuess total = total + 1 - findPowTwoCandidate total

maxGuess :: Int -> Int
maxGuess total = ceiling (fromIntegral total / 2 :: Double)

listPowTwos :: Int -> [Int]
listPowTwos limit = [2 ^ i | i <- [0 .. limit] :: [Int]]

findPowTwoCandidate :: Int -> Int
findPowTwoCandidate total =
  maximum $ filter (powTwoCandidateCriteria total) (listPowTwos total)

powTwoCandidateCriteria :: Int -> Int -> Bool
powTwoCandidateCriteria total val = val <= total && total - val <= maxGuess total

getHumanInput :: Int -> IO String
getHumanInput total = do
  putStrLn $
    "Enter a number between 1 and " ++
    show (maxGuess total :: Int)
  getLine

reportScore :: Int -> IO ()
reportScore num = putStrLn $ "The count is now " ++ show num

playHumanTurn :: Int -> IO ()
playHumanTurn total = do
  putStrLn $ whoseTurn Human ++ " turn"
  num <- getHumanInput total
  reportScore $ total - read num
  playNim (total - read num) Computer

playComputerTurn :: Int -> IO ()
playComputerTurn total = do
  putStrLn $ whoseTurn Computer ++ " turn"
  let computerChoice = getSmartGuess total
  putStrLn $ "Computer chose: " ++ show computerChoice
  reportScore (total - computerChoice)
  playNim (total - computerChoice) Human

playNim :: Int -> User -> IO ()
playNim total userTurn
  | total <= 0 = putStrLn (whoseTurn userTurn ++ " wins!")
  | userTurn == Human = playHumanTurn total
  | otherwise = playComputerTurn total
