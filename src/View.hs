{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where


import Model
import Classes ( Drawable(toPicture), Positionable (getPosition), Collideable (getHitbox) )

import Graphics.Gloss ( scale, translate, color, Picture, white, pictures, text, line, red )
import Graphics.Gloss.Data.Bitmap ()

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@GameState{score, paused, gameOver, timeElapsed, player, activePUs, turrets, drones, kamikazes, playerBullets, enemyBullets, meteors, explosions, sprites, powerUps, bgList}
  | gameOver = objects -- Don't draw player when game is over
  | otherwise = pictures [toPicture sprites player, objects]
  where objects = pictures (drawBG bgList sprites
                            ++ map (toPicture sprites) playerBullets
                            ++ map (toPicture sprites) enemyBullets
                            ++ map (toPicture sprites) turrets
                            ++ map (toPicture sprites) drones
                            ++ map (toPicture sprites) kamikazes
                            ++ map (toPicture sprites) meteors
                            ++ map (toPicture sprites) explosions
                            ++ map (toPicture sprites) powerUps
                            ++ drawUI paused gameOver timeElapsed score (fst (playerHp player)) activePUs sprites
                          )

-- Draw UI elements like the current score and instructions/explanations
drawUI :: Bool -> Bool -> Float -> Score -> Int -> [PowerUpType] -> Sprites -> [Picture]
drawUI paused gameOver timeElapsed (Score score _ _) playerHp activePUs sprites
  | gameOver = (translate (-120) (-90) . color white . scale 0.15 0.15 . text) "Press [Enter] to restart" 
               : (translate (-200) 0 . color white . scale 0.5 0.5 . text) "Game Over!" 
               : loadGameText
               : ui
  | paused = getPausedText timeElapsed ++ ui ++ drawPuInfo sprites
  | otherwise = ui
  where
    ui = [(translate (-100) 280 . color white . scale 0.15 0.15 . text) ("Score: " ++ show score),
          (translate 30 280 . color white . scale 0.15 0.15 . text) ("Hp: " ++ show (max playerHp 0))
         ] ++ drawPUs activePUs

-- Get the text to display when game is paused (differs depending on if game has been started yet)
getPausedText :: Float -> [Picture]
getPausedText timeElapsed
  | timeElapsed == 0 = [(translate (-100) (-90) . color white . scale 0.15 0.15 . text) "Press [Esc] to start",
                        (translate (-200) 150 . color white . scale 0.5 0.5 . text) "Shoot-em-up",
                        (translate (-125) 100 . color white . scale 0.15 0.15 . text) "Thijn Kroon & Mike Wu"]
  | otherwise = [(translate (-120) (-90) . color white . scale 0.15 0.15 . text) "Press [Esc] to resume",
                 (translate (-120) (-140) . color white . scale 0.15 0.15 . text) "Press [O] to save game",
                 loadGameText,
                 (translate (-125) 0 . color white . scale 0.5 0.5 . text) "Paused"]

loadGameText :: Picture
loadGameText = (translate (-120) (-175) . color white . scale 0.15 0.15 . text) "Press [P] to load game"

-- Draw the active power ups
drawPUs :: [PowerUpType] -> [Picture]
drawPUs ps = snd (foldl (\(i, l) x -> (i + 1, drawPU x i : l)) (0, []) ps) where
  drawPU :: PowerUpType -> Float -> Picture
  drawPU pu i = (translate (-495) (280 - i * 30) . color white . scale 0.15 0.15 . text) (show pu)

-- Display info about the kind power ups
drawPuInfo :: Sprites -> [Picture]
drawPuInfo Sprites{hpPowerUp, speedPowerUp, frPowerUp, invincPowerUp} = [
  (translate 180 40 . scale 1.3 1.3) hpPowerUp,
  (translate 180 0 . scale 1.3 1.3) speedPowerUp,
  (translate 180 (-40) . scale 1.3 1.3) frPowerUp,
  (translate 180 (-80) . scale 1.3 1.3) invincPowerUp,
  (translate 210 35 . color white . scale 0.1 0.1 . text) "Replenishes 50 hp",
  (translate 210 (-5) . color white . scale 0.1 0.1 . text) "Increases speed by 20% for 5 seconds",
  (translate 210 (-45) . color white . scale 0.1 0.1 . text) "Increases fire rate by 50% for 5 seconds",
  (translate 210 (-85) . color white . scale 0.1 0.1 . text) "Grants invincibility for 5 seconds"
  ]

drawBG :: [Background] -> Sprites -> [Picture]
drawBG bgList sprites = map draw' bgList
  where draw' bg@(Background pos _ 0) = translate pos 0 (head $ backgroundSprites sprites)
        draw' bg@(Background pos _ 1) = translate pos 0 (backgroundSprites sprites !! 1)
        draw' bg@(Background pos _ _) = translate pos 0 (head $ backgroundSprites sprites)

-- Draw the hitbox of a given entity (used for testing)
drawHbox :: (Positionable a, Collideable a) => a -> Picture
drawHbox a = color red (line [(x-w,y-h),(x+w,y-h),(x+w,y+h),(x-w,y+h),(x-w,y-h)]) where
    (x,y) = getPosition a
    (w,h) = getHitbox a