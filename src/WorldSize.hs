{-# LANGUAGE NoImplicitPrelude #-}

module WorldSize
    ( Location(..)
    ) where

import           Imajuscule.Prelude

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)
