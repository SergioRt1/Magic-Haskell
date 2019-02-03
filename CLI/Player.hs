module Magic.Player where

import Magic.Card

data Player = Player
  { life :: Int
  , manaPool :: Int
  , deck :: [Card]
  , hand :: [Card]
  , table :: [Card]
  , idPlayer :: Int
  , initialCards :: Int
}

instance Show Player where
  show (Player l m d h t id ini) = "Player #" ++ show id ++ ": Life: " ++ show l ++ "Mana Pool: " ++ show m