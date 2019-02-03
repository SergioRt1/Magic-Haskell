module Player where

  import Card

  data Player = Player
    { life :: Int
    , manaPool :: Int
    , hand :: [Card]
    , idPlayer :: Int
  }