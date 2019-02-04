module Main where

import Magic
import Magic.Structure
import Magic.Library

import System.IO
import System.Random
import System.Random.Shuffle


shuffleDeck :: RandomGen g => Library -> g -> Library
shuffleDeck deck gen = shuffle' deck 3 gen

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welecome to Magic-Haskell"
  putStrLn "Made by: Sergio Andres Rodriguez Torres"
  gen <- getStdGen
  setStdGen (snd (next gen))
  runGame [shuffleDeck (getLibrary 0) gen, shuffleDeck (getLibrary 1) gen]


