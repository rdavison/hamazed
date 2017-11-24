{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( run
    ) where

import           Imajuscule.Prelude

import Geo(zeroCoords)
import Animation(mkAnimation, renderAnimation, animatedNumber, mkAnimationTree, Speed(..))
import Timing(getCurrentTime, KeyTime(..))
import WorldSize(Location(..))

run :: IO ()
run = do
  t <- getCurrentTime
  let keytime = KeyTime t
      anim = mkAnimation (animatedNumber 1 (mkAnimationTree zeroCoords)) keytime (Speed 1)
      fLoc _ = InsideWorld
  putStrLn "Before rendering animations"
  _ <- renderAnimation fLoc anim
  putStrLn "After rendering animations"
  return ()
