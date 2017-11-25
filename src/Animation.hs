{-# LANGUAGE NoImplicitPrelude #-}

module Animation
    ( Animation(..)
    , mkAnimationTree
    , renderAnimation
    -- | animations
    , animatedNumber
    ) where


import           Imajuscule.Prelude

newtype Coords = Coords Int

polyExtremities :: Float
                -- ^ If this parameter is not used ,the problem disappears
                -> [Coords]
polyExtremities startAngle = [Coords $ floor startAngle]

newtype Animation = Animation (IO Animation)
-- Note : this is extremely simplified, the previous simplification state was
-- newtype Animation = Animation (Animation -> IO Animation)

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
renderAnimation (Animation render) =
    void render

animatedNumber :: Int -> Tree -> IO Animation
animatedNumber n =
  animate' (mkAnimator animateNumberPure animatedNumber n)

data Animator a = Animator {
    _animatorPure :: !(Tree -> Tree)
  , _animatorIO   :: !(Tree -> IO Animation)
}

mkAnimator :: (t -> [Coords])
           -> (t
               -> Tree
               -> IO Animation)
           -> t
           -> Animator a
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params)

-- with INLINE pragma here, the problem disappears
animate' :: Animator a -> Tree -> IO Animation
animate' (Animator pure_ io_) = animate pure_ io_

animate :: (Tree -> Tree)
        -- ^ the pure animation function
        -> (Tree -> IO Animation)
        -- ^ the IO animation function
        -> Tree
        -> IO Animation
animate pureAnim ioAnim state = do
  let newState = pureAnim state
  ioAnim newState
