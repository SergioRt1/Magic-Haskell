module Magic.Library where

import Magic.Player
import Magic.Card

libraryCards :: [Library]
libraryCards = [ [(Card "Guerrero minotauro" "Creatura Minotauro" "T" False Creature (Just $ Power 2 3) $ Just 3 )]
  , [(Card "Guerrero minotauro2" "Creatura Minotauro" "T" False Creature (Just $ Power 2 3) $ Just 3 )]
  ]

getLibrary :: Int -> Library
getLibrary n = libraryCards !! n