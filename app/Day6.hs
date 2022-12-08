module Day6 where

import AocUtil   (getChallengeData, printPart)
import Data.List (nub)

findNConsecutiveLettersIndex :: String -> Int -> Int -> Int
findNConsecutiveLettersIndex s n n2
    | length s < n2 = 0 
    | length (nub (take n2 s)) == n2 = n + n2
    | otherwise = findNConsecutiveLettersIndex (tail s) (n + 1) n2

run = do
  challengeData <- getChallengeData 6 False
  printPart 1 $ findNConsecutiveLettersIndex (challengeData !! 0) 0 4
  printPart 2 $ findNConsecutiveLettersIndex (challengeData !! 0) 0 14