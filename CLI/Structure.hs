{-# LANGUAGE TemplateHaskell, TypeOperators #-}  --Language option to perform lens setters and getters

module Magic.Structure where


import Magic.Card
import Magic.Player

import Control.Lens hiding (element)

import Data.List
import Data.IORef

--  Game structure

data Game = Game
  { _players :: [Player]
  , _currentPlayer :: Int
  , _steps :: [Step]
  , _record ::[Record]
  }

instance Show Game where
  show (Game p currP s r) = show p ++ "\n" ++ "Player #" ++ show currP

-- Step structure

turnSteps :: [Step]
turnSteps =                  --Step structure for real game magic
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

dummySteps :: [Step]                   --Dummy implementation
dummySteps = [MoveTo]

data Record = Record
  { player :: Int
  , movement :: String
  , card :: Card
  }

instance Show Record where
  show (Record p m c) = "Player #" ++ show p ++ " moved:\n    " ++ show c ++ "\n   " ++ m

data Step = Mulligan MulliganStep
  | Beginning BeginningStep
  | MainPhase
  | Combat CombatStep
  | EndPhase
  | MoveTo
  | InvalidStep InvalidAction
  deriving (Eq, Ord, Show)

data MulliganStep = Take
  | NoCard
  deriving (Eq,Ord,Show)

data BeginningStep = TakeCard
  | RotateCard
  | DrawStep
  deriving (Eq,Ord,Show)

data CombatStep = DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  deriving (Eq,Ord,Show)

data InvalidAction = InvalidInput
  | CantPlayCard
  | EmptyStack
  deriving (Eq,Ord,Show)
-- Lenses maker

makeLenses ''Game
makeLenses ''Player
makeLenses ''Card

getCurrentPlayer :: Game -> Player
getCurrentPlayer (Game p c s r) = p !! c

getNextPlayer :: Game -> Int
getNextPlayer (Game p c s r) = mod (c + 1) (length p)

newPlayers :: [Library] -> [Player]
newPlayers decks = [(Player 20 0 (library x) (hand x) [] [] (snd x) 7) | x <- zip decks [0..]]
  where
    hand x = take 7 (fst x)
    library x = drop 7 (fst x)

newGame :: [Library] -> Game
newGame decks = Game
  { _players = newPlayers decks
  , _currentPlayer = 0
  , _steps = ((Mulligan Take):dummySteps) ++ ((Mulligan Take):(cycle dummySteps))
  , _record = []
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
  let t = tail $ _steps game
  ioStep game s commandRef
  command <- readIORef commandRef
  let nextGame = doStep game s command -- Logic part Game -> Game
  ioPost nextGame s command
  if t /= [] then nextStep nextGame commandRef else return ()

-- Logic of the game

doStep :: Game -> Step -> Int -> Game
doStep game (Beginning TakeCard) command = game

doStep game (Mulligan Take) n = if n == 0 then (set steps remainingSteps game) else
  if ini == 1 then set steps ((Mulligan NoCard):remainingSteps) game else  nextGame
  where
      (x:y:xs) = remainingSteps
      remainingSteps = tail (view steps game)
      nextGame = set (players . ix current) (Player l m library hand_ c t idp (ini-1)) game
      current = view currentPlayer game
      (Player l m deck xHand c t idp ini) = getCurrentPlayer game
      fullDeck = deck ++ xHand
      library = drop (ini - 1) fullDeck
      hand_ = take (ini - 1) fullDeck

doStep (Game p c (x:y:xs) r) (Mulligan NoCard) n = (Game p nextPlayer xs r)
  where
    nextPlayer = getNextPlayer (Game p c xs r)

doStep game MoveTo n =
  case n of
    0 -> game
    1 -> playCard game h tab
    2 -> moveCard game hand h cemetery c " from hand to cementery"
    3 -> moveCard game hand h library lib " from hand to library"
    4 -> moveCard game library lib hand h " from library to hand"
    5 -> moveCard game library lib table tab " from library to table"
    6 -> moveCard game library lib cemetery c " from library to cemetery"
    7 -> moveCard game cemetery c hand h " from cemetery to hand"
    8 -> moveCard game cemetery c table tab " from cemetery to table"
    9 -> moveCard game cemetery c library lib " from cemetery to library"
    where
      (Player l m lib h c tab idp ini) = getCurrentPlayer game

doStep game (InvalidStep _) n = set steps remainingSteps game
  where remainingSteps = tail (view steps game)

moveCard :: Game -> ((a0 -> Identity [Card])-> Player -> Identity Player) -> [Card] -> ((a1 -> Identity [Card])-> Player -> Identity Player) -> [Card] -> String -> Game
moveCard game f1 from f2 target text = if from == [] then set steps ((InvalidStep EmptyStack):currentSteps) game
  else Game players2 (getNextPlayer game) remainingSteps $ record ++ [(Record current text card)]
    where
      card = head from
      rest = tail from
      (Game players current steps_ record) = game
      players1 = set (ix current . f1) rest players
      players2 = set (ix current . f2) (card:target) players1
      currentSteps = (view steps game)
      remainingSteps = tail currentSteps



playCard :: Game -> Hand -> [Card] -> Game
playCard game hand_ table_ = if hand_ == [] then set steps ((InvalidStep EmptyStack):currentSteps) game else
  if (manaNeed card) == Nothing then doMove
  else if canMove card player then doMove else set steps ((InvalidStep CantPlayCard):currentSteps) game
    where
      doMove = moveCard game hand hand_ table table_ " from hand to table"
      Just n = manaNeed card
      card = head hand_
      player = getCurrentPlayer game
      currentSteps = (view steps game)

canMove :: Card -> Player -> Bool
canMove card player = if isSimpleRequirement requirement  then (mana requirement) <= (length lands)
  else (mana requirement) <= ( (length lands) - (length (special requirement) ) ) && extract (special requirement) lands
    where
      lands = [(color x) | x <- tab, (cardType x) == Land]
      tab = view table player
      (Just requirement) = (manaNeed card)

isSimpleRequirement :: Requirement -> Bool
isSimpleRequirement (SimpleRequirement _) = True
isSimpleRequirement _ = False

extract :: [Color] -> [Color] -> Bool
extract [] [] = True
extract [] x = True
extract x [] = False
extract (x:xs) l2 =  if elem x l2 then extract xs (delete x l2) else False

-- I/O Stuff

-- ioStep for initial I/O each step

ioStep :: Game -> Step -> IORef Int -> IO ()
ioStep _ (Beginning TakeCard) _ = do
  putStrLn $ show (Beginning TakeCard)

ioStep game (Mulligan Take) command = do
  printCards game
  putStrLn "Do you like to do mulligan?\n0) No\n1) Yes\n(enter only the corresponding number)"
  readSafeInt game (Mulligan Take) command


ioStep game (Mulligan NoCard) command = do
  putStrLn $ "Sorry player#" ++ show (view currentPlayer game) ++ " you do not have cards."

ioStep game (MoveTo) command = do
  printCards game
  putStrLn "Select an option"
  putStrLn "0) View record"
  putStrLn "1) Move Card from hand to table"
  putStrLn "2) Move Card from hand to cementery"
  putStrLn "3) Move Card from hand to library"
  putStrLn "4) Move Card from library to hand"
  putStrLn "5) Move Card from library to table"
  putStrLn "6) Move Card from library to cemetery"
  putStrLn "7) Move Card from cemetery to hand"
  putStrLn "8) Move Card from cemetery to table"
  putStrLn "9) Move Card from cemetery to library"
  putStrLn "(enter only the corresponding number and the movement of the first card from the source to the destination will be made )"
  readSafeInt game (MoveTo) command

ioStep game (InvalidStep EmptyStack) command = do
  putStrLn "The source where you are going to move the card is empty\nTry again"

ioStep game (InvalidStep CantPlayCard) command = do
  putStrLn "You can not move this card to the table\nTry again"

-- ioStep Helper functions

readSafeInt :: Game -> Step -> IORef Int -> IO ()
readSafeInt game step command = do
  c <- getLine
  case validInputInt c of
      Just n -> writeIORef command n
      Nothing -> do
        putStrLn "Invalid input :(, try again"
        ioStep game step command

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

ioPost game (Mulligan Take) command = do
  return ()

ioPost game (Mulligan NoCard) command = do return ()

ioPost game _ 0 = do
  putStrLn "The record is:"
  printRecord (view record game)

ioPost game _ command = do return ()

printRecord :: [Record] -> IO ()
printRecord [] = return ()
printRecord (x:xs) = do
  putStrLn $ show x
  printRecord xs