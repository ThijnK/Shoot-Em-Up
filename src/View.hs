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
viewPure gstate@GameState{player, turrets, drones, playerBullets, enemyBullets, obstacles, explosions, sprites} = 
    pictures (map (toPicture sprites) playerBullets
              ++ map (toPicture sprites) enemyBullets
              ++ map (toPicture sprites) turrets
              ++ map (toPicture sprites) drones
              ++ map (toPicture sprites) obstacles 
              ++ map (toPicture sprites) explosions
              ++ [toPicture sprites player]
              -- ++ map drawHbox enemyBullets
              -- ++ map drawHbox turrets
            )
