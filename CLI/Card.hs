module Magic.Card where

  data Card = Card{
   name:: String
    , description::String
    , expansion :: String
    , _used :: Bool
    , cardType :: CardType
    , _power :: Maybe Power
    , color :: Color
    , manaNeed :: Maybe Requirement
  }


  instance Eq Card where
    x == y = (cardType x == cardType y) && (_power x == _power y)

  instance Show Card where
    show (Card n d e u t p c m) = "Card name: " ++ n ++ " Type: " ++ show t ++ " Desc: " ++ d ++ " Exp: " ++ e ++ " Color: " ++ show c ++ " " ++ show m ++ " " ++ evalMaybe p

  rotate :: Card -> Card
  rotate (Card n d e u t p c m) = Card n d e (not u) t p c m

  data Power =  Power Int  Int deriving (Eq)

  instance Show Power where
    show (Power a b) = "Power: " ++ show a ++ " " ++ show b

  data Requirement =
    SimpleRequirement {
      mana :: Int
    } | SpecialRequirement{
       mana :: Int
     , special :: [Color]
  } deriving (Eq)

  instance Show Requirement where
    show (SpecialRequirement m s) = "Requirement: mana = " ++ show m ++ " special =" ++sList s
    show (SimpleRequirement m) = "Requirement: mana = " ++ show m

  evalMaybe :: (Show a) => Maybe a -> String
  evalMaybe (Just n) = show n
  evalMaybe Nothing = ""

  sList :: (Show a) => [a] -> String
  sList [] = ""
  sList (x:xs) = " " ++ show x ++ sList xs

  data CardType = Creature | Land | Enchantment | Instant deriving (Show,Eq)

  data Color = Red | Green | Blue | Black | White deriving (Show,Eq)

