module Main where

import Magic
import Magic.Structure
import Magic.Library

import System.IO


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welecome to Magic-Haskell"
  putStrLn "Made by: Sergio Andres Rodriguez Torres"
  runGame [getLibrary 0, getLibrary 1]


