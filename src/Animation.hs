{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Animation
    ( Animation(..)
    , mkAnimation
    , mkAnimationTree
    , renderAnimation
    -- | animations
    , animatedNumber
    ) where


import           Imajuscule.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Control.Exception( assert )

import           Geo( Coords
                    , polyExtremities )
import           WorldSize(Location(..))


data Animation = Animation {
    _animationRender :: !(Animation -> (Coords -> Location) -> IO (Maybe Animation))
}

mkAnimation :: (Animation -> (Coords -> Location) -> IO (Maybe Animation))
            -> Animation
mkAnimation render = Animation render

-- \ This datastructure is used to keep a state of the animation progress, not globally,
--   but locally on each animation point. It is also recursive, so that we can sequence
--   multiple animations.
data Tree = Tree {
    _treeRoot :: !Coords
    -- ^ where the animation begins
  , _treeBranches :: !(Maybe [Either Tree Coords])
    -- ^ There is one element in the list per animation point.
    -- 'Right Coords' elements are still alive (typically they didn't collide yet with the world).
    -- 'Left Tree' elements are dead for this animation and maybe gave birth to another animation.
}

mkAnimationTree :: Coords -> Tree
mkAnimationTree c = Tree c Nothing

combine :: [Coords]
        -> [Either Tree Coords]
        -> (Coords -> Location)
        -> [Either Tree Coords]
combine points uncheckedPreviousState getLocation =
  let previousState = assert (length points == length uncheckedPreviousState) uncheckedPreviousState
  in zipWith (combinePoints getLocation) points previousState

combinePoints :: (Coords -> Location)
              -> Coords
              -> Either Tree Coords
              -> Either Tree Coords
combinePoints _ point =
  either Left (\_ -> Right point)

applyAnimation :: (Coords -> [Coords])
               -> (Coords -> Location)
               -> Tree
               -> Tree
applyAnimation animation getLocation (Tree root branches) =
  let points = animation root
      previousState = fromMaybe (replicate (length points) $ Right $ assert (getLocation root == InsideWorld) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState getLocation
  in Tree root $ Just newBranches

animateNumberPure :: Int -> Coords -> [Coords]
animateNumberPure nSides _ =
  let startAngle = if odd nSides then pi else pi/4.0
  in polyExtremities startAngle -- replacing startAngle by pi or (pi/4.0) fixes the problem


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

renderAnimation :: (Coords -> Location) -> Animation -> IO ()
renderAnimation getLocation a@(Animation render) =
    void( render a getLocation )

setRender :: Animation
          -> (Animation -> (Coords -> Location) -> IO (Maybe Animation))
          -> Animation
setRender (Animation _) = Animation

animatedNumber :: Int -> Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animatedNumber n =
  animate' (mkAnimator animateNumberPure animatedNumber n)

data Animator a = Animator {
    _animatorPure :: !((Coords -> Location) -> Tree -> Tree)
  , _animatorIO   :: !(Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
}

mkAnimator :: (t -> Coords -> [Coords])
           -> (t
               -> Tree
               -> Animation
               -> (Coords -> Location)
               -> IO (Maybe Animation))
           -> t
           -> Animator a
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params)

-- when inlining this function the problem disappears
--{-# INLINE animate' #-}
animate' :: Animator a -> Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animate' (Animator pure_ io_) = animate pure_ io_

animate :: ((Coords -> Location) -> Tree -> Tree)
        -- ^ the pure animation function
        -> (Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
        -- ^ the IO animation function
        ->  Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animate pureAnim ioAnim state a@(Animation _) getLocation = do
  let newState = pureAnim getLocation state
  return $ Just (setRender a $ ioAnim newState)
