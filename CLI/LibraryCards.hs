module Magic.Library where

import Magic.Player
import Magic.Card

libraryCards :: [Library]
libraryCards = [ [(Card "Guerrero minotauro" "Creatura Minotauro" "T" False Creature (Just $ Power 2 3) $ Just 3 )
                  , (Card "Mandibula maldita" "Arroladora cratura tiene gran defensa y resistencia" "F" False Creature (Just $ Power 5 5) $ Just 5 )
                  , (Card "Isla" "Tierra azul basica" "F" False Land Nothing Nothing)
                  ]
  , [(Card "Guerrero minotauro2" "Creatura Minotauro" "T" False Creature (Just $ Power 2 3) $ Just 3 )
    , (Card "Isla" "Tierra azul basica" "F" False Land Nothing Nothing)
    , (Card "Volcan" "Tierra Roja basica" "T" False Land Nothing Nothing)
  ]
  ]

getLibrary :: Int -> Library
getLibrary player = libraryCards !! player