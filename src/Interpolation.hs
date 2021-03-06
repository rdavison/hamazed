{-# LANGUAGE NoImplicitPrelude #-}

module Interpolation
         ( DiscretelyInterpolable(..)
         -- | types to select a particular instance of DiscretelyInterpolable
         , SequentiallyInterpolatedList(..)
         , Successive(..)
         -- | Reexports
         , module Iteration
         ) where

import           Imajuscule.Prelude

import           Data.List( length, mapAccumL )

import           Iteration
import           Math


newtype Successive a = Successive [a] deriving(Show)

-- | The instances of this class should statisfy the following constraints:
--
-- For every {from, to} <- v
--
-- > d = distance from to
--
-- > interpolate from to 0 == from
-- > interpolate from to d == to
--
--   "The interpolation path is composed of distinct points"
--
-- > length $ nubOrd $ (interpolate from to) map [from..to] == d
--
--   "Given any points A,B belonging the path generated by an interpolation,
--     the interpolation beween A and B will be the points of the path between A and B"
--
-- For every med in [0..d]
--
-- > distance from med + distance med to == 1 + distance from to
-- > medVal = interpolate from to med
--
-- For every low  in [0..med]
-- For every high in [med..d]
--
-- > interpolate from to low  == interpolate from medVal low
-- > interpolate from to high == interpolate medVal to $ high-med
class (Show v) => DiscretelyInterpolable v where

  -- | Special case where the interpolation is between 2 values
  distance :: v -- ^ first value
           -> v -- ^ last value
           -> Int -- ^ the number of steps (including first and last) to go from first to last

  -- | General case where the interpolation goes through n values
  distanceSuccessive :: Successive v
                     -> Int
  distanceSuccessive (Successive []) =
    error "empty successive"
  distanceSuccessive (Successive l@(_:_)) =
    succ $ sum $ zipWith (\a b -> pred $ distance a b) l $ tail l

  interpolate :: v -- ^ first value
              -> v -- ^ last value
              -> Int -- ^ the current step
              -> v -- ^ the interpolated value
  interpolate = error "interpolate is not defined"

  interpolate' :: v -- ^ first value
               -> v -- ^ last value
               -> Int -- ^ the current step
               -> w -- ^ the interpolated value
  interpolate' = error "interpolate' is not defined"

  interpolateIO :: v -- ^ first value
                -> v -- ^ last value
                -> Int -- ^ the current step
                -> IO ()
  interpolateIO = error "interpolateIO is not defined"

  interpolateSuccessive :: Successive v
                        -> Int
                        -> v
  interpolateSuccessive (Successive []) _ = error "empty successive"
  interpolateSuccessive (Successive [a]) _ = a
  interpolateSuccessive (Successive l@(a:b:_)) i
    | i <= 0      = a
    | i >= lf = interpolateSuccessive (Successive $ tail l) $ i-lf
    | otherwise = interpolate a b i
    where lf = pred $ distance a b

  interpolateSuccessive' :: Successive v
                         -> Int
                         -> w
  interpolateSuccessive' (Successive []) _ = error "empty successive"
  interpolateSuccessive' (Successive [a]) _ = interpolate' a a 0
  interpolateSuccessive' (Successive l@(a:b:_)) i
    | i <= 0      = interpolate' a a 0
    | i >= lf = interpolateSuccessive' (Successive $ tail l) $ i-lf
    | otherwise = interpolate' a b i
    where lf = pred $ distance a b

  interpolateSuccessiveIO :: Successive v
                          -> Int
                          -> IO ()
  interpolateSuccessiveIO (Successive []) _ = error "empty successive"
  interpolateSuccessiveIO (Successive [a]) _ = interpolateIO a a 0
  interpolateSuccessiveIO (Successive l@(a:b:_)) i
    | i <= 0      = interpolateIO a a 0
    | i >= lf = interpolateSuccessiveIO (Successive $ tail l) $ i-lf
    | otherwise = interpolateIO a b i
    where lf = pred $ distance a b


instance DiscretelyInterpolable Int where
  distance i i' =
    1 + abs (i-i')
  interpolate i i' progress =
    i + signum (i'-i) * clamp progress 0 (abs (i-i'))


-- | Interpolation between 2 lists, occuring in parallel between same-index elements.
--   Prerequisite : lists have the same lengths.
--
--  For an interpolation that occurs sequentially between same-index elements,
--   use SequentiallyInterpolatedList.
instance (DiscretelyInterpolable a)
      => DiscretelyInterpolable ([] a) where
  distance [] _ = 1
  distance _ [] = 1
  distance l l' =
    maximum $ zipWith distance l $ assert (length l == length l') l'

  interpolate l l' progress =
    zipWith (\e e' -> interpolate e e' progress) l $ assert (length l == length l') l'


newtype SequentiallyInterpolatedList a =
  SequentiallyInterpolatedList [a]
  deriving(Eq, Ord, Show)

-- | Interpolation between 2 SequentiallyInterpolatedList, occuring sequentially
--   between same-index elements.
--   Prerequisite : lists have the same lengths.
--
--  For an interpolation that occurs in parallel, use [].
instance (DiscretelyInterpolable a)
      => DiscretelyInterpolable (SequentiallyInterpolatedList a) where

  distance (SequentiallyInterpolatedList l) (SequentiallyInterpolatedList l') =
    succ $ sum $ zipWith (\x y -> pred $ distance x y) l (assert (length l' == length l) l')

  interpolate (SequentiallyInterpolatedList l) (SequentiallyInterpolatedList l') progress =
    SequentiallyInterpolatedList $ snd $
      mapAccumL
        (\acc (e,e') ->
          let d = pred $ distance e e'
              r = interpolate e e' $ clamp acc 0 d
          in (acc-d, r))
        progress
        $ zip l (assert (length l' == length l) l')
