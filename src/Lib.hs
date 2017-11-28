module Lib
    ( run
    ) where

import Animation(Animation(..), renderAnimation, animatedNumber, mkAnimationTree)

run :: IO ()
run = do
  let anim = Animation (animatedNumber 1 mkAnimationTree)
  putStrLn "Before rendering animations"
  _ <- renderAnimation anim
  putStrLn "After rendering animations"
  return ()
