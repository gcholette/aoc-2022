module Day2 where

import AocUtil   (getChallengeData, printPart)
import Data.List (splitAt)

data Outcome = Win  | Draw  | Lose    deriving (Eq)
data Shape   = Rock | Paper | Scisors deriving (Eq)

shapesList = [Rock, Paper, Scisors]

shapeScore :: Shape -> Int 
shapeScore Rock    = 1
shapeScore Paper   = 2
shapeScore Scisors = 3

char2Shape :: Char -> Shape
char2Shape chr 
  | chr == 'A' || chr == 'X' = Rock
  | chr == 'B' || chr == 'Y' = Paper
  | chr == 'C' || chr == 'Z' = Scisors

char2Outcome :: Char -> Outcome
char2Outcome 'X' = Lose
char2Outcome 'Y' = Draw
char2Outcome 'Z' = Win

outcomeScore :: Outcome -> Int
outcomeScore Win  = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

playRound1 :: (Shape, Shape) -> Outcome
playRound1 (them, you) 
  | them == you = Draw
  | you  == shapesList !! ((shapeScore them) `mod` 3) = Win
  | otherwise = Lose

playRound2 :: (Shape, Outcome) -> Shape
playRound2 (them, outcome) 
  | outcome == Draw = them 
  | outcome == Win  = shapesList !! ((shapeScore them) `mod` 3)
  | outcome == Lose = shapesList !! ((shapeScore them - 2) `mod` 3)

calculateScore :: [(Outcome, Shape)] -> Int
calculateScore xs = sum $ map(\(outcome, item) -> (outcomeScore outcome) + (shapeScore item)) xs
  
parseChallengeData :: [String] -> [(Char, Char)]
parseChallengeData = map (\x -> (x !! 0, x !! 2))

parsePart1 :: [(Char, Char)] -> [(Shape, Shape)]
parsePart1 = map(\(x, y) -> (char2Shape x, char2Shape y))

parsePart2 :: [(Char, Char)] -> [(Shape, Outcome)]
parsePart2 = map(\(x, y) -> (char2Shape x, char2Outcome y))

run = do
  challengeData <- getChallengeData 2 False
  let parsedChallengeData1 = parsePart1 $ parseChallengeData challengeData
  let scoreSum1 = calculateScore $ map (\x -> (playRound1 x, snd x)) parsedChallengeData1
  printPart 1 scoreSum1

  let parsedChallengeData2 = parsePart2 $ parseChallengeData challengeData
  let scoreSum2 = calculateScore $ map (\x -> (snd x, playRound2 x)) parsedChallengeData2
  printPart 2 scoreSum2
