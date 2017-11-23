{-# LANGUAGE NoImplicitPrelude #-}

module Windows(
    getCharWithinMicros
  , flushStdin
  ) where

import Imajuscule.Prelude

import Prelude(getChar)

import System.IO(stdin, hReady)
import Control.Concurrent(threadDelay)
import Control.Monad.Loops(iterateUntilM)
import Data.Char(Char)
import Data.Maybe(isNothing)

import Timing(getCurrentTime, diffTimeSecToMicros, diffUTCTime, UTCTime)

data State = State !UTCTime !(Maybe Char)

-- on windows, unbuffered input doesn't work, cf. https://ghc.haskell.org/trac/ghc/ticket/2189
-- so once we detected some input is available we must send a newline char to trigger a 
-- flush of stdin else getChar blocks until a newline character is pressed
-- TODO on non-windows, make this be a no op
flushStdin :: IO ()
flushStdin = sendInputNewLine 

sendInputNewLine :: IO()
sendInputNewLine = undefined -- TODO mimic ://stackoverflow.com/questions/19578565/simulating-keystrokes-with-haskell-on-windows

getCharWithinMicros :: Int -> IO (Maybe Char)
getCharWithinMicros allowedMicros = do
  start <- getCurrentTime
  (State _ res) <- iterateUntilM 
    (\(State t mc) -> isNothing mc && (diffTimeSecToMicros (diffUTCTime t start)) < allowedMicros)
    (\_ -> (do
        ready <- hReady stdin
        if ready
          then do
             flushStdin
             c <- Prelude.getChar
             t <- getCurrentTime
             return (State t (Just c))
          else do
             threadDelay 5000
             t <- getCurrentTime
             return (State t Nothing))
       )
    $ State start Nothing
  return res

