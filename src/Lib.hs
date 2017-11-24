{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( run
    ) where

import           Imajuscule.Prelude

import Geo(zeroCoords)
import Animation(mkAnimation, renderAnimation, animatedNumber, mkAnimationTree)
import WorldSize(Location(..))

run :: IO ()
run = do
  let anim = mkAnimation (animatedNumber 1 (mkAnimationTree zeroCoords))
      fLoc _ = InsideWorld
  putStrLn "Before rendering animations"
  _ <- renderAnimation fLoc anim
  putStrLn "After rendering animations"
  return ()
