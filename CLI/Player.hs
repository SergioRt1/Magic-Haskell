module Magic.Player where

  import Magic.Card

  type Hand = [Card]

  type Library = [Card]

  type Cementery = [Card]

  data Player = Player
    { _life :: Int
    , _manaPool :: Int
    , _library :: Library
    , _hand :: Hand
    , _cementery :: Cementery
    , _table :: [Card]
    , _idPlayer :: Int
    , _initialCards :: Int
    , _invalidAction :: Bool
  }

  instance Show Player where
    show (Player l m _ _ _ _ id _ _) = "Player #" ++ show id ++ ": Life: " ++ show l ++ " ManaPool: " ++ show m


