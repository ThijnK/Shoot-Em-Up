{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where


import Model
import Classes ( Drawable(toPicture), drawHbox )

import Graphics.Gloss ( scale, translate, color, Picture, white, pictures, text )
import Graphics.Gloss.Data.Bitmap ()

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@GameState{gameOver, player, turrets, drones, kamikazes, playerBullets, enemyBullets, meteors, explosions, sprites}
  | gameOver = objects -- Don't draw player when game is over
  | otherwise = pictures [objects, toPicture sprites player]
  where objects = pictures (map (toPicture sprites) playerBullets
                            ++ map (toPicture sprites) enemyBullets
                            ++ map (toPicture sprites) turrets
                            ++ map (toPicture sprites) drones
                            ++ map (toPicture sprites) kamikazes
                            ++ map (toPicture sprites) meteors
                            ++ map (toPicture sprites) explosions
                            ++ drawUI gstate
                          )

drawUI :: GameState -> [Picture]
drawUI gstate@GameState{player, paused, gameOver, score = Score n _ _}
  | gameOver = (translate (-120) (-90) . color white . scale 0.15 0.15 . text) "Press [Enter] to restart" : (translate (-200) 0 . color white . scale 0.5 0.5 . text) "Game Over!" : ui
  | paused = (translate (-120) (-90) . color white . scale 0.15 0.15 . text) "Press [Esc] to resume"
           : (translate (-120) (-140) . color white . scale 0.15 0.15 . text) "Press [O] to save game"
           : (translate (-120) (-175) . color white . scale 0.15 0.15 . text) "Press [P] to load game"
           : (translate (-125) 0 . color white . scale 0.5 0.5 . text) "Paused"
           : ui
  | otherwise = ui
  where ui = [(translate (-100) 280 . color white . scale 0.15 0.15 . text) ("Score: " ++ show n),
              (translate 30 280 . color white . scale 0.15 0.15 . text) ("Hp: " ++ show (max (playerHp player) 0))
             ]
