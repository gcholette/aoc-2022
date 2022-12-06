module Day1 where

import AocUtil   (getChallengeData, isStrDefined, printPart)
import Data.List (groupBy, sort)

parseChallengeData :: [String] -> [Int]
parseChallengeData xs =
  map (\x -> sum $ map read $ filter isStrDefined x) $
  groupBy (\x -> isStrDefined) xs

run :: IO ()
run = do
  challengeData <- getChallengeData 1 False
  let parsedData = parseChallengeData challengeData
  printPart 1 $ maximum parsedData
  printPart 2 $ sum $ take 3 $ reverse $ sort $ parsedData
