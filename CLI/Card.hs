module Magic.Card where

  data Card = Card {
    name:: String
    , description::String
    , expansion :: String
    , _used :: Bool
    , cardType :: CardType
    , _power :: Maybe Power
    , manaNeed :: Maybe Int
  }

  instance Eq Card where
    x == y = (cardType x == cardType y) && (_power x == _power y)

  instance Show Card where
    show (Card n d e u t p _) = "Card name: " ++ n ++ " Type: " ++ show t ++ " Desc: " ++ d ++ " Exp: " ++ e

  rotate :: Card -> Card
  rotate (Card n d e u c p m) = Card n d e (not u) c p m

  data Power =  Power Int  Int deriving (Eq)

  instance Show Power where
    show (Power a b) = " Power: " ++ show a ++ " " ++ show b

  data CardType = Creature | Land | Enchantment | Instant deriving (Show,Eq)

--  useAction :: Player -> Card -> Player
--  useAction ()
