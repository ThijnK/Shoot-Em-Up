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
viewPure gstate@GameState{player, playerBullets, obstacles, sprites} = 
    pictures (map (toPicture sprites) playerBullets
              ++ [toPicture sprites player]
              ++ map (toPicture sprites) obstacles) where
    pSprite = playerSprite sprites
    pbSprite = pBulletSprite sprites
    oSprite = obstacleSprite sprites
{-
              ++ [temp oSprite (Obstacle (0,0) 100 (10,10))]-}

-- temporary for testing
temp :: (Positionable a, Collideable a) => Picture -> a -> Picture
temp sprite a = pictures [translate x y sprite, color red (line [(x-w,y-h),(x+w,y-h),(x+w,y+h),(x-w,y+h),(x-w,y-h)])] where
    (x,y) = getHitbox a
    (w,h) = getPosition a