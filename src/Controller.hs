{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Char
import Data.Maybe
import Model

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState{player, playerBullets, downKeys} = return gstate' where
  gstate' = checkCollision (gstate{player = p, playerBullets = pbs}) pbs
  (Just p)   = move gstate player
  (Just pbs) = move gstate playerBullets

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = fireBullet gstate
inputKey (EventKey (Char c) d _ _) gstate
  | c == 'w' || c == 'a' || c == 's' || c == 'd' = updateKeys d
  | otherwise = gstate
  where
    updateKeys :: KeyState -> GameState
    updateKeys Down = gstate { downKeys = c : downKeys gstate }
    updateKeys Up   = gstate { downKeys = delete c (downKeys gstate) }
inputKey (EventKey (MouseButton LeftButton) Up _ _) gstate = fireBullet gstate
inputKey _ gstate = gstate

fireBullet :: GameState -> GameState
fireBullet gstate@GameState{player, playerBullets} = gstate {playerBullets = friendlyBullet origin : playerBullets} where
  origin = playerPos player -- take the player's position as the origin of the bullet

friendlyBullet :: Point -> PlayerBullet
friendlyBullet origin = PlayerBullet origin 10 50 (10,2)
