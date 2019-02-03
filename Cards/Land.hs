module Card
  ( Card(..)
  ,  rotate
  ) where

  import Card

  data Land = Land {
                  name:: String
                  , cardType::String
                  , description::String
                  , expansion :: String
                  , used :: Bool
                }