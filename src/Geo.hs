{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Geo ( Coords(..)
           , zeroCoords
           , polyExtremities
           ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

newtype Row = Row { _rowIndex :: Int } deriving (Generic, Eq, Show, Ord)
newtype Col = Col { _colIndex :: Int } deriving (Generic, Eq, Show, Ord)

data Coords = Coords {
    _x :: !Row
  , _y :: !Col
} deriving (Generic, Eq, Show, Ord)

{-# INLINE zeroCoords #-}
zeroCoords :: Coords
zeroCoords = Coords (Row 0) (Col 0)

-- if this function doesn't use the Float (startAngle) the problem disappears
polyExtremities :: Float -> [Coords]
polyExtremities startAngle = [Coords (Row (floor startAngle)) (Col 0)]
