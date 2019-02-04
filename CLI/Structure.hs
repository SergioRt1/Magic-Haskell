module Magic.Structure where

import Magic.Card
import Magic.Player

import Data.IORef

--  Game structure

data Game = Game
  { players :: [Player]
  , currentPlayer :: Int
  }

instance Show Game where
  show (Game p currP) = show p ++ "\n" ++ "Player #" ++ show currP

setPlayer :: Game -> Int -> Player-> Game
setPlayer (Game players c) n newPlayer = Game (take n players ++ [newPlayer] ++ drop (n+1) players) c

getCurrentPlayer :: Game -> Player
getCurrentPlayer game = (players game) !! (currentPlayer game)


getHand :: Library -> Int -> Hand
getHand lib n = take n lib

takeFromDeck :: Library -> Hand -> Library
takeFromDeck deck hand = [ x | x <- deck, not $ elem x hand]

newPlayers :: [Library] -> [Player]
newPlayers decks = [(Player 20 0 (library x) (hand x) [] [] (snd x) 7 False) | x <- zip decks [0..]]
  where
    hand x = getHand (fst x) 7
    library x = takeFromDeck (fst x) $ hand x

newGame :: [Library] -> Game
newGame decks = Game
  { players = newPlayers decks
  , currentPlayer = 0
  }

-- Turn-Steps structure

runGame :: [Library] -> IO ()
runGame decks = do
  commandRef <- newIORef (0 :: Int)      -- IO input Game -> I/O
  nextStep game steps commandRef
  where
    game = newGame decks
    steps = Mulligan:turnSteps ++ Mulligan:(cycle turnSteps)


nextStep :: Game -> [Step] -> IORef Int -> IO ()
nextStep game (s:t) commandRef = do
  ioStep game commandRef s
  command <- readIORef commandRef
  let nextGame = doStep game s command -- Logic part Game -> Game
  ioPost nextGame s
  nextStep (nextGame) t commandRef

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

--data EndPhase = EndOfTurnStep
--  deriving (Eq,Ord,Show)

-- Logic of the game

doStep :: Game -> Step -> Int -> Game
doStep game (Beginning TakeCard) command = game

--doStep game Mulligan n =

--moveCardToTable :: IORef Player -> IO ()
--moveCardToTable refPlayer = do
--  (Player l mana lib hand table id ini invalid) <- readIORef refPlayer


-- I/O Stuff

-- ioStep for initial I/O each step

ioStep :: Game -> IORef Int -> Step -> IO ()
ioStep _ _ (Beginning TakeCard) = do
  putStrLn $ show (Beginning TakeCard)

ioStep game command Mulligan = do
  printCards game
  putStrLn "Do you like to do mulligan?\n0) No\n1) Yes"
  c <- getLine
  case validInputInt c of
    Just n -> writeIORef command n
    Nothing -> do
      putStrLn "Invalid input :(, try again"
      ioStep game command Mulligan

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
  printEachCard (zip (hand player) [1..])

printEachCard :: [(Card, Int)] -> IO ()
printEachCard [] = return ()
printEachCard (x:xs) = do
  putStrLn $ show (snd x) ++ ") " ++ show (fst x)
  printEachCard xs


-- ioPost for final I/O each step

ioPost :: Game -> Step -> IO ()
ioPost game (Beginning TakeCard) = do
  putStrLn $ "The card taken was: " ++ show (head h)
  where
    player = getCurrentPlayer game
    h = hand player
