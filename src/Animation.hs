{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation
    ( Animation(..)
    , Speed(..)
    , mkAnimation
    , mkAnimationTree
    , earliestDeadline
    , renderAnimation
    -- | animations
    , animatedNumber
    ) where


import           Imajuscule.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           GHC.Generics( Generic )
import           Control.Exception( assert )

import           Geo( Coords
                    , polyExtremities )
import           Timing( KeyTime )
import           WorldSize(Location(..))


newtype Iteration = Iteration (Speed, Frame) deriving(Generic, Eq, Show)
newtype Frame = Frame Int deriving(Generic, Eq, Show, Num)
newtype Speed = Speed Int deriving(Generic, Eq, Show, Num)

{-# INLINE zeroIteration #-}
zeroIteration :: Speed -> Iteration
zeroIteration s = Iteration (s,zeroFrame)

{-# INLINE zeroFrame #-}
zeroFrame :: Frame
zeroFrame = Frame 0

data Animation = Animation {
    _animationNextTime :: !KeyTime
  , _animationIteration :: !Iteration
  , _animationRender :: !(Animation -> (Coords -> Location) -> IO (Maybe Animation))
}

mkAnimation :: (Animation -> (Coords -> Location) -> IO (Maybe Animation))
            -> KeyTime
            -> Speed
            -> Animation
mkAnimation render t speed = Animation t {-do not increment, it will be done while rendering-} (zeroIteration speed) render

-- \ This datastructure is used to keep a state of the animation progress, not globally,
--   but locally on each animation point. It is also recursive, so that we can sequence
--   multiple animations.
data Tree = Tree {
    _treeRoot :: !Coords
    -- ^ where the animation begins
  , _treeStart :: !Frame
    -- ^ when the animation begins (relatively to the parent animation if any)
  , _treeBranches :: !(Maybe [Either Tree Coords])
    -- ^ There is one element in the list per animation point.
    -- 'Right Coords' elements are still alive (typically they didn't collide yet with the world).
    -- 'Left Tree' elements are dead for this animation and maybe gave birth to another animation.
}

mkAnimationTree :: Coords -> Tree
mkAnimationTree c = Tree c 0 Nothing

combine :: [Coords]
        -> [Either Tree Coords]
        -> Iteration
        -> (Coords -> Location)
        -> [Either Tree Coords]
combine points uncheckedPreviousState iteration getLocation =
  let previousState = assert (length points == length uncheckedPreviousState) uncheckedPreviousState
  in zipWith (combinePoints getLocation iteration) points previousState

combinePoints :: (Coords -> Location)
              -> Iteration
              -> Coords
              -> Either Tree Coords
              -> Either Tree Coords
combinePoints _ _ point =
  either Left (\_ -> Right point)

applyAnimation :: (Coords -> Frame -> [Coords])
               -> Iteration
               -> (Coords -> Location)
               -> Tree
               -> Tree
applyAnimation animation iteration@(Iteration (_,globalFrame)) getLocation (Tree root startFrame branches) =
  let frame = globalFrame - startFrame
      points = animation root frame
      previousState = fromMaybe (replicate (length points) $ Right $ assert (getLocation root == InsideWorld) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration getLocation
  in Tree root startFrame $ Just newBranches

animateNumberPure :: Int -> Coords -> Frame -> [Coords]
animateNumberPure nSides _ _ =
  let startAngle = if odd nSides then pi else pi/4.0
  in polyExtremities startAngle -- replacing startAngle by pi or (pi/4.0) fixes the problem

earliestDeadline :: [Animation] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(Animation deadline _ _) -> deadline) animations


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

renderAnimation :: (Coords -> Location) -> Animation -> IO ()
renderAnimation getLocation a@(Animation _ _ render) =
    void( render a getLocation )

setRender :: Animation
          -> (Animation -> (Coords -> Location) -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

animatedNumber :: Int -> Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animatedNumber n =
  animate' (mkAnimator animateNumberPure animatedNumber n)

data Animator a = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> Tree -> Tree)
  , _animatorIO   :: !(Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
}

mkAnimator :: (t -> Coords -> Frame -> [Coords])
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

animate :: (Iteration -> (Coords -> Location) -> Tree -> Tree)
        -- ^ the pure animation function
        -> (Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
        -- ^ the IO animation function
        ->  Tree -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animate pureAnim ioAnim state a@(Animation _ i _) getLocation = do
  let newState = pureAnim i getLocation state
  return $ Just (setRender a $ ioAnim newState)
