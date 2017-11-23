{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module NonBlockingIO
    ( tryGetChar
    ) where

import           Imajuscule.Prelude

import qualified Prelude (getChar)
import           System.IO( hReady
                          , stdin)

import           Windows(flushStdin)

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

callIf :: IO a -> IO Bool -> IO (Maybe a)
callIf call condition =
  condition >>= \case
    True  -> (Just <$> call)
    False -> (return Nothing)

tryGetChar :: IO (Maybe Char)
tryGetChar = (flushStdin >> Prelude.getChar) `callIf` someInputIsAvailable
  where
    someInputIsAvailable = hReady stdin
