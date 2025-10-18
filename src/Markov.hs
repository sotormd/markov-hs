module Markov
  (
    Prefix
  , Chain
  , buildChain
  , generateText
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (tails)
import Data.Char (isAlpha, toLower)
import System.Random (randomRIO)

-- list of last n words
type Prefix = [String]

-- the markov chain map
type Chain = Map Prefix [String]

-- sanitize input
sanitize :: String -> String
sanitize = unwords . words . map cleanChar
  where
    cleanChar c
      | isAlpha c = toLower c
      | otherwise = ' '

-- build the markov chain
buildChain :: Int -> String -> Chain
buildChain n text = foldl insert Map.empty ngrams
  where
    wordsList = words (sanitize text)
    ngrams = [ (take n xs, xs !! n) | xs <- tails wordsList, length xs > n ]

    insert :: Map Prefix [String] -> (Prefix, String) -> Map Prefix [String]
    insert m (prefix, next) = Map.insertWith (++) prefix [next] m

-- generate text from chain
generateText :: Chain -> Int -> IO String
generateText chain len
  | Map.null chain = return ""
  | otherwise = do
      let prefixes = Map.keys chain
      startIdx <- randomRIO (0, length prefixes - 1)
      let start = prefixes !! startIdx
      go start len start
  where
    go :: Prefix -> Int -> [String] -> IO String
    go _ 0 acc = return $ unwords acc
    go prefix n acc = case Map.lookup prefix chain of
      Nothing -> return $ unwords acc
      Just nexts -> do
        nextIdx <- randomRIO (0, length nexts - 1)
        let next = nexts !! nextIdx
            newPrefix = drop 1 prefix ++ [next]
        go newPrefix (n - 1) (acc ++ [next])
