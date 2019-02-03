module Magic.Structure where

import Magic.Card
import Magic.Player

import Data.IORef

--  Game structure

data Game = Game
  { player1 :: Player
  , player2 :: Player
  , actualStepList :: [Step]
  , currentPlayer :: Int
  }

instance Show Game where
  show (Game p1 p2 stpList currP) = show p1 ++ "\n" ++ show p2 ++ "\n" ++ "Player #" ++ show currP ++ "What do you like to do?"

getCurrentPlayer :: Game -> Player
getCurrentPlayer game = if (currentPlayer game) == 1 then (player1 game) else (player2 game)


type Deck = [Card]

type Library = [Card]

type Cementery = [Card]

type Hand = [Card]

getHand :: Deck -> Int -> Hand
getHand d n = take n d

takeFromDeck :: Deck -> Hand -> Deck
takeFromDeck deck hand = [ x | x <- deck, not $ elem x hand]

newGame :: [Deck] -> Game
newGame (deck1:deck2:[]) = Game
  { player1 = (Player 20 0 d1 (hand1) [] 1 7)
  , player2 = (Player 20 0 d2 (hand2) [] 2 7)
  , actualStepList = cycle turnSteps
  , currentPlayer = 1
  }where
    hand1 = getHand deck1 7
    hand2 = getHand deck2 7
    d1 = takeFromDeck deck1 hand1
    d2 = takeFromDeck deck2 hand2

-- Turn-Steps structure

runGame :: [Deck] -> IO ()
runGame decks = do
  nextStep game steps
  where
    steps = cycle turnSteps
    game = newGame decks


nextStep :: Game -> [Step] -> IO ()
nextStep game (s:t) = do
  commandRef <- newIORef (0 :: Int)
  ioStep commandRef s                -- IO Part Game -> I/O
  command <- readIORef commandRef
  let nextGame = doStep game s command -- Logic part Game -> Game
  ioPost nextGame s
  nextStep (nextGame) t

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

data Step = Beginning BeginningStep
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


-- I/O Stuff

ioStep :: IORef Int -> Step -> IO ()
ioStep _ (Beginning TakeCard) = do
  putStrLn $ show (Beginning TakeCard)


ioPost :: Game -> Step -> IO ()
ioPost game (Beginning TakeCard) = do
  putStrLn $ "The card taken was: " ++ show (head h)
  where
    player = getCurrentPlayer game
    h = hand player
