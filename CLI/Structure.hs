{-# LANGUAGE TemplateHaskell, TypeOperators #-}  --Language option to perform setters and getters

module Magic.Structure where


import Magic.Card
import Magic.Player

import Control.Lens hiding (element)

import Data.IORef

--  Game structure

data Game = Game
  { _players :: [Player]
  , _currentPlayer :: Int
  , _steps :: [Step]
  }

instance Show Game where
  show (Game p currP s) = show p ++ "\n" ++ "Player #" ++ show currP

-- Step structure

turnSteps :: [Step]
turnSteps =
  [ Beginning TakeCard
  , Beginning RotateCard
  , Beginning DrawStep
  , MainPhase
  , Combat DeclareAttackersStep
  , Combat DeclareBlockersStep
  , Combat CombatDamageStep
  , MainPhase
  , EndPhase
  ]

data Step = Mulligan
  | Beginning BeginningStep
  | MainPhase
  | Combat CombatStep
  | EndPhase
  deriving (Eq, Ord, Show)

data BeginningStep = TakeCard
  | RotateCard
  | DrawStep
  deriving (Eq,Ord,Show)

data CombatStep = DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  deriving (Eq,Ord,Show)

-- Lenses maker

makeLenses ''Game
makeLenses ''Player
makeLenses ''Card

getCurrentPlayer :: Game -> Player
getCurrentPlayer (Game p c s) = p !! c

newPlayers :: [Library] -> [Player]
newPlayers decks = [(Player 20 0 (library x) (hand x) [] [] (snd x) 7 False) | x <- zip decks [0..]]
  where
    hand x = take 7 (fst x)
    library x = drop 7 (fst x)

newGame :: [Library] -> Game
newGame decks = Game
  { _players = newPlayers decks
  , _currentPlayer = 0
  , _steps = Mulligan:turnSteps ++ Mulligan:(cycle turnSteps)
  }

-- Turn-Steps structure

runGame :: [Library] -> IO ()
runGame decks = do
  commandRef <- newIORef (0 :: Int)      -- IO input Game -> I/O
  nextStep game commandRef
  where
    game = newGame decks


nextStep :: Game -> IORef Int -> IO ()
nextStep game commandRef = do
  let s = head $ _steps game
  ioStep game s commandRef
  command <- readIORef commandRef
  let nextGame = doStep game s command -- Logic part Game -> Game
  ioPost nextGame s command
  nextStep nextGame commandRef

--data EndPhase = EndOfTurnStep
--  deriving (Eq,Ord,Show)

-- Logic of the game

doStep :: Game -> Step -> Int -> Game
doStep game (Beginning TakeCard) command = game

doStep game Mulligan n = if n == 0 then (set steps (tail (view steps game)) game) else nextGame
  where
      nextGame = set (players . ix current) (Player l m library hand_ c t idp (ini-1) inv) game
      current = view currentPlayer game
      (Player l m deck xHand c t idp ini inv) = getCurrentPlayer game
      hand_ = take (ini - 1) deck
      library = (drop (ini - 1) deck) ++ xHand

--moveCardToTable :: IORef Player -> IO ()
--moveCardToTable refPlayer = do
--  (Player l mana lib hand table id ini invalid) <- readIORef refPlayer


-- I/O Stuff

-- ioStep for initial I/O each step

ioStep :: Game -> Step -> IORef Int -> IO ()
ioStep _ (Beginning TakeCard) _ = do
  putStrLn $ show (Beginning TakeCard)

ioStep game Mulligan command = do
  printCards game
  putStrLn "Do you like to do mulligan?\n0) No\n1) Yes\n(enter only the corresponding number)"
  c <- getLine
  case validInputInt c of
    Just n -> writeIORef command n
    Nothing -> do
      putStrLn "Invalid input :(, try again"
      ioStep game Mulligan command

-- ioStep Helper functions

validInputInt :: String -> Maybe Int
validInputInt s =
  case reads s :: [(Int,String)] of
     [(n, _)] -> Just n
     otherwise -> Nothing

printCards :: Game -> IO ()
printCards game = do
  let player = getCurrentPlayer game
  putStrLn $ show player
  putStrLn "Your hand is:"
  printEachCard (zip (view hand player) [1..])

printEachCard :: [(Card, Int)] -> IO ()
printEachCard [] = return ()
printEachCard (x:xs) = do
  putStrLn $ show (snd x) ++ ") " ++ show (fst x)
  printEachCard xs


-- ioPost for final I/O each step

ioPost :: Game -> Step -> Int -> IO ()
ioPost game (Beginning TakeCard) _ = do
  putStrLn $ "The card taken was: " ++ show (head h)
  where
    player = getCurrentPlayer game
    h = view hand player

ioPost game Mulligan command = do
  return ()
