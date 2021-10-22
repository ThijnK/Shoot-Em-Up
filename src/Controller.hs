-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate { player = updatePlayer } where
  updatePlayer = Player h (x + mx, y + my) s f hb
  (mx, my) = calcMovement s (downKeys gstate) -- how much the x and y position of the player should change
  (Player h (x,y) s f hb) = player gstate
  

-- Calculate the x and y movement based on the keys that are currently held down
calcMovement :: Float -> [Char] -> (Float, Float)
calcMovement s = foldr f (0,0) where
  f 'w' (x,y) = (x, y + s)
  f 's' (x,y) = (x, y - s)
  f 'a' (x,y) = (x - s, y)
  f 'd' (x,y) = (x + s, y)
  f _   acc   = acc

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) d _ _) gstate = updateKeys d where
  updateKeys Down = gstate { downKeys = c : downKeys gstate }
  updateKeys Up   = gstate { downKeys = delete c (downKeys gstate)}
inputKey _ gstate = gstate