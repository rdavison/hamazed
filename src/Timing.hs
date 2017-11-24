{-# LANGUAGE NoImplicitPrelude #-}

module Timing
    ( addGameStepDuration
    , addAnimationStepDuration
    , computeTime
    , diffTimeSecToMicros
    , Timer(..)
    , KeyTime(..)
    -- | reexports
    , UTCTime(..)
    , diffUTCTime
    , addUTCTime
    , getCurrentTime
    ) where

import           Imajuscule.Prelude

import           Data.Time( addUTCTime
                          , diffUTCTime
                          , getCurrentTime
                          , NominalDiffTime
                          , UTCTime(..) )

-- I introduce this type to prevent equality test which make no sense, like
-- between "current system time" and a time that was computed
newtype KeyTime = KeyTime UTCTime deriving(Eq, Ord, Show)

diffTimeSecToMicros :: NominalDiffTime -> Int
diffTimeSecToMicros t = floor (t * 10^(6 :: Int))


newtype Timer = Timer { _initialTime :: UTCTime }

computeTime :: Timer -> UTCTime -> Int
computeTime (Timer t1) t2 =
  let t = diffUTCTime t2 t1
  in floor t


-- the console can refresh at approx. 21 fps, hence this value (1/25)
animationPeriod :: NominalDiffTime
animationPeriod = 0.04

gamePeriod :: NominalDiffTime
gamePeriod = fromIntegral gamePeriodMicros / 1000000

addGameStepDuration :: KeyTime -> KeyTime
addGameStepDuration = addDuration gamePeriod

addAnimationStepDuration :: KeyTime -> KeyTime
addAnimationStepDuration = addDuration animationPeriod

addDuration :: NominalDiffTime -> KeyTime -> KeyTime
addDuration durationSeconds (KeyTime t) = KeyTime $ addUTCTime durationSeconds t

-- using the "incremental" render backend, there is no flicker
-- using the "full" render backend, flicker starts at 40
gamePeriodMicros :: Int
gamePeriodMicros = gamePeriodMillis * 1000
  where
    gamePeriodMillis = 160 -- this controls the game loop frequency.
                           -- 20 seems to match screen refresh frequency
