

runGame :: [Deck] -> [Step]
runGame a (x:xs) = runGame $  turnSteps x game
  s


turnSteps :: [Step]
turnSteps =
  [ Beginning TakeCard
  , Beginning RotateCard
  , Beginning DrawStep
  , MainStep
  , Combat DeclareAttackersStep
  , Combat DeclareBlockersStep
  , Combat CombatDamageStep
  , MainPhase
  , EndPhase EndOfTurnStep
  ]

data Step
  = Beginning BeginningStep
  | MainStep
  | Combat CombatStep
  | EndPhase
  deriving (Eq, Ord, Show)

data BeginningStep
  = TakeCard
  | RotateCard
  | DrawStep
  deriving (Eq,Ord,Show,Enum)

data CombatStep
  = DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  deriving (Eq,Ord,Show,Enum)

data EndPhase = EndOfTurnStep
  deriving (Eq,Ord,Show,Enum)

doStep :: Step -> Game -> Game
doStep (Beginning TakeCard) =