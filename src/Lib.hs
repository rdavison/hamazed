{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( run
    ) where

import           Imajuscule.Prelude

import Animation(mkAnimation, renderAnimation, animatedNumber, mkAnimationTree)

run :: IO ()
run = do
  let anim = mkAnimation (animatedNumber 1 mkAnimationTree)
  putStrLn "Before rendering animations"
  _ <- renderAnimation anim
  putStrLn "After rendering animations"
  return ()
