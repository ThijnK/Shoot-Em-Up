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

-- secs * speed to normalize

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState{player, playerBullets, downKeys, obstacles} = return gstate' where
  -- gstate' = gstate {player = p}
  -- gstate' = checkCollision (gstate{player = p, playerBullets = pbs}) pbs
  gstate' = foldr (\x acc -> stuff (shoot x obstacles) acc) gstate{player = p, playerBullets = pbs} playerBullets where
    stuff (Nothing, Nothing) acc = acc
    stuff (Just pb, Nothing) acc = remove pb acc
    stuff (Just pb, Just o) acc = (remove pb . remove o) acc
    stuff _ acc = acc
  p = movePlayer player downKeys
    --map (shoot obstacles) playerBullets
  pbs = map move playerBullets
    
  -- (Just pbs) = move gstate playerBullets

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
friendlyBullet origin = PlayerBullet origin 0 10 50 (10,2)

movePlayer :: Player -> [Char] -> Player
movePlayer player@Player{playerPos = (x,y), playerSpeed} downKeys = player {playerPos = (clamp (x + mx) (-500,500), clamp (y + my) (-300,300))} where
    (mx,my) = foldr checkKey (0,0) downKeys -- Move based on the keys currently being held down
    checkKey :: Char -> (Float, Float) -> (Float, Float)
    checkKey 's' (x,y) = (x, y - playerSpeed)
    checkKey 'a' (x,y) = (x - playerSpeed, y)
    checkKey 'w' (x,y) = (x, y + playerSpeed)
    checkKey 'd' (x,y) = (x + playerSpeed, y)
    checkKey _   acc   = acc
