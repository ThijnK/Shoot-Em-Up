{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@GameState{player, playerBullets, obstacles, explosions, sprites} = 
    pictures (map (toPicture sprites) playerBullets
              ++ [toPicture sprites player]
              ++ map (toPicture sprites) obstacles 
              ++ map (toPicture sprites) explosions
              -- ++ [temp (Obstacle (0,0) 0 50 (10,10))]
            )

              

-- temporary for testing
temp :: (Positionable a, Collideable a) => a -> Picture
temp a = color red (line [(x-w,y-h),(x+w,y-h),(x+w,y+h),(x-w,y+h),(x-w,y-h)]) where
    (x,y) = getPosition a
    (w,h) = getHitbox a