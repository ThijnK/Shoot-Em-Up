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
viewPure gstate@GameState{gameOver, player, turrets, drones, playerBullets, enemyBullets, meteors, explosions, sprites}
  | gameOver = objects -- Don't draw player when game is over
  | otherwise = pictures [toPicture sprites player, objects]
  where objects = pictures (map (toPicture sprites) playerBullets
                            ++ map (toPicture sprites) enemyBullets
                            ++ map (toPicture sprites) turrets
                            ++ map (toPicture sprites) drones
                            ++ map (toPicture sprites) meteors 
                            ++ map (toPicture sprites) explosions
                            ++ drawUI gstate
                            -- ++ map drawHbox turrets
                          )

drawUI :: GameState -> [Picture]
drawUI gstate@GameState{gameOver, score} 
  | gameOver = (translate (-200) 0 . color white . scale 0.5 0.5 . text) "Game Over!" : ui
  | otherwise = ui
  where ui = [(translate (-495) 290 . color white . scale 0.1 0.1 . text) ("Score: " ++ show score)]
