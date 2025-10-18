module Main (main) where

import Markov

main :: IO ()
main = do
  input <- getContents
  let chain = buildChain 2 input
  text <- generateText chain 100

  putStrLn text

