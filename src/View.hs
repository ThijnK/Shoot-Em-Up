-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = movePlayer (player gstate) where
  movePlayer (Player _ (x,y) _ _ _) = translate x y (color blue (circle 10))