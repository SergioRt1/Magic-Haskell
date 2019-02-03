module Magic where

  import Card

  data Game = Game
  { players :: [Player]
  , actualStep :: Step
  }

  type Deck = [Card]

  type Library = [Card]

  type Cementery = [Card]

