module AocUtil where

printPart :: (Show a1, Show a2) => a1 -> a2 -> IO ()
printPart n x = putStrLn $ ("Part "<>show n<>": ") ++ show x

getChallengeData :: Int -> Bool -> IO [String]
getChallengeData day isTest = do
  lines <$> readFile ("./inputs/input_" ++ dayStr ++ extension)
  where
    dayStr = show day
    extension = if isTest then "_test.txt" else ".txt"

isStrDefined :: Foldable t => t a -> Bool
isStrDefined s = length s > 0