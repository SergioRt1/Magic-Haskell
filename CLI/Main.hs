module Main where

import Magic
--import Magic.Structure

import System.IO


main :: IO ()
main = do {
    hSetBuffering stdout NoBuffering
  ; putStrLn "Welecome to Magic-Haskell"
  ; putStrLn "Made by Sergio Andres Rodriguez Torres"
  ; option <- newIORef (0 :: Int)
  runGame
}

