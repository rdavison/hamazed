{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( run
    ) where

import           Imajuscule.Prelude

import Geo(zeroCoords)
import Animation(mkAnimation, renderAnimation, animatedNumber, mkAnimationTree)

run :: IO ()
run = do
  let anim = mkAnimation (animatedNumber 1 (mkAnimationTree zeroCoords))
  putStrLn "Before rendering animations"
  _ <- renderAnimation anim
  putStrLn "After rendering animations"
  return ()
