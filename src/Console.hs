{-# LANGUAGE NoImplicitPrelude #-}

module Console ( ConsoleConfig(..)
               , configureConsoleFor
               -- rendering functions
               , beginFrame
               , endFrame
               , setForeground
               , setRawForeground
               , restoreForeground
               , renderChar_
               , renderStr
               , renderStr_
               , renderTxt
               , renderTxt_
               , RenderState(..)
               , renderSegment
               -- reexport System.Console.ANSI
               , ColorIntensity(..)
               , Color(..)
               ) where

import           Imajuscule.Prelude

import           Data.Maybe( fromMaybe )
import           Data.String( String )
import           Data.Text( Text, unpack )

import qualified System.Console.Terminal.Size as Terminal( size
                                                         , Window(..))

import           System.Console.ANSI( clearScreen
                                    , hideCursor
                                    , setSGR
                                    , setCursorPosition
                                    , showCursor
                                    , ColorIntensity(..)
                                    , Color(..) )
import           System.IO( hSetBuffering
                          , hSetEcho
                          , BufferMode( .. )
                          , stdin
                          , stdout )


import           Geo( Col(..)
                    , Coords(..)
                    , Segment(..)
                    , sumCoords
                    , translateInDir
                    , Direction(..)
                    , Row(..) )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data ConsoleConfig = Gaming | Editing

newtype RenderState = RenderState {
    _currentUpperLeftCorner :: Coords
}

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

configureConsoleFor :: ConsoleConfig -> IO ()
configureConsoleFor config = do
  hSetEcho stdin $ case config of
      Gaming  -> False
      Editing -> True
  case config of
    Gaming  -> do
      hideCursor
      clearScreen
    Editing -> do
      showCursor
      -- do not clearScreen, to retain a potential printed exception
      setSGR []
      maySz <- Terminal.size
      let (Terminal.Window x _) = fromMaybe (Terminal.Window 0 0) maySz
      setCursorPosition x 0

beginFrame :: IO ()
beginFrame = return ()

endFrame :: IO ()
endFrame = return ()

restoreForeground :: Int -> IO ()
restoreForeground _ = return ()

setForeground :: ColorIntensity -> Color -> IO Int
setForeground _ _ = return 0

setRawForeground :: Int -> IO Int
setRawForeground _ = return 0

renderChar_ :: Char -> RenderState -> IO ()
renderChar_ char (RenderState c) = do
  putChar char
  return ()


renderStr :: String -> RenderState -> IO RenderState
renderStr str r@(RenderState c) =
  renderStr_ str r >> return (RenderState $ translateInDir Down c)

renderStr_ :: String -> RenderState -> IO ()
renderStr_ str (RenderState c) =
  putStr str

renderTxt :: Text -> RenderState -> IO RenderState
renderTxt txt r@(RenderState c) =
  renderTxt_ txt r >> return (RenderState $ translateInDir Down c)

renderTxt_ :: Text -> RenderState -> IO ()
renderTxt_ txt (RenderState c) =
  putStr $ unpack txt

renderSegment :: Segment -> Char -> RenderState -> IO ()
renderSegment l = case l of
  Horizontal row c1 c2 -> renderHorizontalLine row c1 c2
  Vertical col r1 r2   -> renderVerticalLine   col r1 r2
  Oblique _ _ -> error "oblique segment rendering is not supported"


renderVerticalLine :: Col -> Int -> Int -> Char -> RenderState -> IO ()
renderVerticalLine col r1 r2 char (RenderState upperLeft) = do
  let rows = [(min r1 r2)..(max r1 r2)]
  mapM_ (renderChar_ char . (\r -> RenderState $ sumCoords upperLeft $ Coords (Row r) col)) rows

renderHorizontalLine :: Row -> Int -> Int -> Char -> RenderState -> IO ()
renderHorizontalLine row c1 c2 char (RenderState upperLeft) =
  renderStr_ (replicate (1 + abs (c2-c1)) char) $ RenderState $ sumCoords upperLeft $ Coords row (Col (min c1 c2))
