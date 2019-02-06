module Magic.Library where

import Magic.Player
import Magic.Card

libraryCards :: [Library]
libraryCards = [ [(Card "Guerrero minotauro" "Creatura Minotauro" "T" False Creature (Just $ Power 3 1) Red $ Just (SimpleRequirement 3) )
                  , (Card "Mandibula maldita" "Arroladora cratura tiene gran defensa y resistencia" "F" False Creature (Just $ Power 5 5) Black $ Just (SpecialRequirement 3 [Black,Red]) )
                  , (Card "Isla" "Tierra azul basica" "F" False Land Nothing Blue Nothing)
                  , (Card "Montaña" "Tierra roja basica" "F" False Land Nothing Red Nothing)
                  , (Card "Bosque" "Tierra verde basica" "F" False Land Nothing Green Nothing)
                  , (Card "Pantano" "Tierra negra basica" "F" False Land Nothing Black Nothing)
                  , (Card "Llanura" "Tierra blanca basica" "F" False Land Nothing White Nothing)
                  , (Card "Muerte lenta" "Hechizo que inflinje 2 de daño instantane al oponente" "F" False Instant Nothing Black $ Just (SimpleRequirement 2))
                  , (Card "Furia del guerrero" "Potencia en 2 el daño de una de tus creaturas" "F" False Enchantment Nothing Red $ Just (SimpleRequirement 2))
                  , (Card "Coraza Tortuga" "Potencia en 2 el la defenza de una de tus creaturas" "F" False Enchantment Nothing Green $ Just (SimpleRequirement 2))
                  ]
  , [(Card "Guerrero minotauro" "Creatura Minotauro" "T" False Creature (Just $ Power 3 1) Red $ Just (SimpleRequirement 3) )
    , (Card "Coraza Tortuga" "Potencia en 2 el la defenza de una de tus creaturas" "F" False Enchantment Nothing Green $ Just (SimpleRequirement 2))
    , (Card "Muerte lenta" "Hechizo que inflinje 2 de daño instantane al oponente" "F" False Instant Nothing Black $ Just (SimpleRequirement 2))
    , (Card "Bosque" "Tierra verde basica" "F" False Land Nothing Green Nothing)
    , (Card "Montaña" "Tierra roja basica" "F" False Land Nothing Red Nothing)
    , (Card "Llanura" "Tierra blanca basica" "F" False Land Nothing White Nothing)
    , (Card "Mandibula maldita" "Arroladora cratura tiene gran defensa y resistencia" "F" False Creature (Just $ Power 5 5) Black $ Just (SpecialRequirement 3 [Black,Red]) )
    , (Card "Pantano" "Tierra negra basica" "F" False Land Nothing Black Nothing)
    , (Card "Isla" "Tierra azul basica" "F" False Land Nothing Blue Nothing)
    , (Card "Furia del guerrero" "Potencia en 2 el daño de una de tus creaturas" "F" False Enchantment Nothing Red $ Just (SimpleRequirement 2))
  ]
  ]

getLibrary :: Int -> Library
getLibrary player = libraryCards !! player