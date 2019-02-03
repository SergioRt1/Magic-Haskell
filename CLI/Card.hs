module Magic.Card where

  data Card = Card {
    name:: String
    , description::String
    , expansion :: String
    , used :: Bool
    , cardType :: CardType
    , power :: Maybe Power
  }

  instance Eq Card where
    x == y = (cardType x == cardType y) && (power x == power y)

  instance Show Card where
    show (Card n d e u t p) = "Card name: " ++ n ++ " Type: " ++ show t ++ " Desc: " ++ d ++ " Exp: " ++ e

  rotate :: Card -> Card
  rotate (Card n d e u c p) = Card n d e (not u) c p

  data Power =  Power Int  Int deriving (Eq)

  instance Show Power where
    show (Power a b) = " Power: " ++ show a ++ " " ++ show b

  data CardType = Creature | Land | Enchantment | Instant deriving (Show,Eq)

--  useAction :: Player -> Card -> Player
--  useAction ()
