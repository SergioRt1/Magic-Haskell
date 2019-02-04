module Magic.Player where

  import Magic.Card

  type Hand = [Card]

  type Library = [Card]

  type Cementery = [Card]

  data Player = Player
    { life :: Int
    , manaPool :: Int
    , library :: Library
    , hand :: Hand
    , cementery :: Cementery
    , table :: [Card]
    , idPlayer :: Int
    , initialCards :: Int
    , invalidAction :: Bool
  }

  instance Show Player where
    show (Player l m _ _ _ _ id _ _) = "Player #" ++ show id ++ ": Life: " ++ show l ++ " ManaPool: " ++ show m


