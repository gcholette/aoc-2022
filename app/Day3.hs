module Day3 where

import Data.List       ( intersect )
import Data.List.Split ( chunksOf )
import Data.Char       ( ord, isLower )
import AocUtil         (getChallengeData, printPart)

commonItemType :: String -> Char
commonItemType s = head $ intersect first second
  where
    (first, second) = splitAt (length s `div` 2) s
  
priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

groupStringsBy3 :: [String] -> [(String, String, String)]
groupStringsBy3 xs = map (\[a,b,c] -> (a,b,c)) $ chunksOf 3 xs

badgeFrom3Bags :: (String, String, String) -> Char
badgeFrom3Bags (b1, b2, b3) = intersect b3 (intersect b1 b2) !! 0

run :: IO ()
run = do
  challengeData <- getChallengeData 3 False
  printPart 1 $ sum $ map (priority . commonItemType) challengeData
  printPart 2 $ sum $ map priority (map badgeFrom3Bags (groupStringsBy3 challengeData))
