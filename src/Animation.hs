
module Animation
    ( Animation(..)
    , mkAnimationTree
    , renderAnimation
    , animatedNumber
    ) where

-- Replacing the lines above by the line hereunder fixes the problem
--module Animation where

import Control.Monad(void)

newtype Coords = Coords Int

polyExtremities :: Float
                -- ^ If this parameter is not used ,the problem disappears
                -> [Coords]
polyExtremities startAngle = [Coords $ floor startAngle]


newtype Animation = Animation (Animation -> IO (Maybe Animation))

newtype Tree = Tree [Coords]

mkAnimationTree :: Tree
mkAnimationTree = Tree []

applyAnimation :: [Coords]
               -- ^ When this parameter is not used, the problem disappears.
               -> Tree
               -- ^ -- When removing this parameter from the signature,
                    --      eventhough the function ignores it, the problem disappears.
               -> Tree
applyAnimation animation _ =
  Tree animation

animateNumberPure :: Int -> [Coords]
animateNumberPure nSides =
  let startAngle = if odd nSides then pi else pi/4.0 -- replacing nSides here by 1 fixes the problem
  in polyExtremities startAngle -- replacing startAngle here by pi or (pi/4.0) fixes the problem


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

renderAnimation :: Animation -> IO ()
renderAnimation a@(Animation render) =
    void( render a )

animatedNumber :: Int -> Tree -> Animation  -> IO (Maybe Animation)
animatedNumber n =
  animate' (mkAnimator animateNumberPure animatedNumber n)


data Animator a = Animator {
    _animatorPure :: !(Tree -> Tree)
  , _animatorIO   :: !(Tree -> Animation  -> IO (Maybe Animation))
    -- ^ removing strictness annotation on _animatorIO fixes the problem
}

mkAnimator :: (t -> [Coords])
           -> (t
               -> Tree
               -> Animation
               -> IO (Maybe Animation))
           -> t
           -> Animator a
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params)

-- when inlining this function the problem disappears
--{-# INLINE animate' #-}
animate' :: Animator a -> Tree -> Animation  -> IO (Maybe Animation)
animate' (Animator pure_ io_) = animate pure_ io_

animate :: (Tree -> Tree)
        -- ^ the pure animation function
        -> (Tree -> Animation  -> IO (Maybe Animation))
        -- ^ the IO animation function
        -> Tree
        -> Animation
        -> IO (Maybe Animation)
animate pureAnim ioAnim state (Animation _) = do
  let newState = pureAnim state
  putStrLn "animation is rendered"
  return $ Just (Animation $ ioAnim newState) -- When returning Nothing here the problem disappears
