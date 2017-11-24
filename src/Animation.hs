{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation
    ( Animation(..)
    , Speed(..)
    , mkAnimation
    , mkAnimationTree
    , earliestDeadline
    , renderAnimations
    -- | animations
    , animatedNumber
    ) where


import           Imajuscule.Prelude

import           Data.List( length )
import           Data.Either( partitionEithers )
import           Data.Maybe( catMaybes
                           , fromMaybe )

import           GHC.Generics( Generic )
import           Control.Exception( assert )

import           Collision( firstCollision )
import           Geo( Coords
                    , bresenham
                    , bresenhamLength
                    , Segment
                    , Direction(..)
                    , mkSegment
                    , move
                    , polyExtremities
                    , rotateCcw
                    , showSegment
                    , translatedFullCircle
                    , translatedFullCircleFromQuarterArc
                    , parabola
                    , Vec2(..)
                    , pos2vec
                    , vec2coords )
import           Resample( resample )
import           Timing( KeyTime
                       , addAnimationStepDuration )
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

{-# INLINE nextIteration #-}
nextIteration :: Iteration -> Iteration
nextIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i + speed))

{-# INLINE previousIteration #-}
previousIteration :: Iteration -> Iteration
previousIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i - speed))

data StepType = Update
              | Same

data Animation = Animation {
    _animationNextTime :: !KeyTime
  , _animationIteration :: !Iteration
  , _animationRender :: !(StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
}

mkAnimation :: (StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
            -> KeyTime
            -> Speed
            -> Animation
mkAnimation render t speed = Animation t {-do not increment, it will be done while rendering-} (zeroIteration speed) render


getStep :: Maybe KeyTime -> Animation -> StepType
getStep mayKey (Animation k' (Iteration(_,frame)) _)
  | frame == zeroFrame = Update -- initialize step
  | otherwise          = maybe Same (\k -> if k == k' then Update else Same) mayKey

applyStep :: StepType -> Animation -> Animation
applyStep = \case
               Update -> stepAnimation
               Same   -> id

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

getAliveCoordinates :: Tree -> [Coords]
getAliveCoordinates (Tree _ _ Nothing) = []
getAliveCoordinates (Tree _ _ (Just [])) = []
getAliveCoordinates (Tree _ _ (Just branches)) =
  let (children, aliveCoordinates) = partitionEithers branches
  in concatMap getAliveCoordinates children ++ aliveCoordinates


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
combinePoints getLocation iteration point =
  either Left (\prevPoint -> let trajectory = bresenham (mkSegment (assert (getLocation prevPoint == InsideWorld) prevPoint) point)
                                 collision =  firstCollision getLocation trajectory
                             in  maybe
                                   (Right $ assert (getLocation point == InsideWorld) point)
                                   (\(_, preCollisionCoords) ->
                                        -- TODO use currentFrame instead of previous and verify combining animations look good:
                                        -- using the previous was an historical choice when there was no notion of trajectory
                                        -- but now, since here we move to the precoliision, it makes sense to not skip a frame
                                        -- anymore
                                        let (Iteration(_,frame)) = previousIteration iteration
                                        in Left $ Tree preCollisionCoords frame Nothing)
                                   collision)

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
animateNumberPure = polygon

polygon :: Int -> Coords -> Frame -> [Coords]
polygon nSides center (Frame i) =
  let startAngle = if odd nSides then pi else pi/4.0
      extremities = polyExtremities nSides center i startAngle
  in connect extremities

connect :: [Coords] -> [Coords]
connect []  = []
connect l@[_] = l
connect (a:rest@(b:_)) = connect2 a b ++ connect rest

connect2 :: Coords -> Coords -> [Coords]
connect2 start end =
  let numpoints = 80 -- more than 2 * (max height width of world) to avoid spaces
  in sampledBresenham numpoints start end

sampledBresenham :: Int -> Coords -> Coords -> [Coords]
sampledBresenham nSamples start end =
  let l = bresenhamLength start end
      seg = mkSegment start end
      bres = bresenham seg
  in resample bres (assert (l == length bres) l) nSamples

stepAnimation :: Animation -> Animation
stepAnimation (Animation t i f) = Animation (addAnimationStepDuration t) (nextIteration i) f

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

renderAnimations :: Maybe KeyTime -> (Coords -> Location) -> [Animation] -> IO [Animation]
renderAnimations k getLocation anims =
  catMaybes <$> mapM (\a@(Animation _ _ render) -> do
    let step = getStep k a
        a' = applyStep step a
    render step a' getLocation) anims

setRender :: Animation
          -> (StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

animatedNumber :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animatedNumber n =
  animate' (mkAnimator animateNumberPure animatedNumber n)

data Animator a = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> Tree -> Tree)
  , _animatorIO   :: !(Tree -> StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
}

mkAnimator :: (t -> Coords -> Frame -> [Coords])
           -> (t
               -> Tree
               -> StepType
               -> Animation
               -> (Coords -> Location)
               -> IO (Maybe Animation))
           -> t
           -> Animator a
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params)

-- if this function is not inlined, in optimized mode, the program loops forever when trigerring the animation. TODO test with latest GHC
--{-# INLINE animate' #-}
animate' :: Animator a -> Tree -> StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animate' (Animator pure_ io_) = animate pure_ io_

animate :: (Iteration -> (Coords -> Location) -> Tree -> Tree)
        -- ^ the pure animation function
        -> (Tree -> StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation))
        -- ^ the IO animation function
        ->  Tree -> StepType -> Animation -> (Coords -> Location) -> IO (Maybe Animation)
animate pureAnim ioAnim state step a@(Animation _ i _) getLocation = do
  let newState = case step of
        Update -> pureAnim i getLocation state
        Same -> state
  renderAnimation (getAliveCoordinates newState) (setRender a $ ioAnim newState)

renderAnimation :: [Coords] -> Animation -> IO (Maybe Animation)
renderAnimation points a = do
  putStrLn "."
  return $ if null points then Nothing else Just a
