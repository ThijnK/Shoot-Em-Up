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
step secs gstate@GameState{paused, player, downKeys, explosions}
  | paused = return gstate -- No change to gamestate if game is paused
  | otherwise = return gstate'
  where
    gstate' = updatePlayerBullets gstate{deltaTime = secs, player = p, explosions = e}
    p = updatePlayer secs player downKeys
    e = updateExplosions secs explosions

-- | Update objects
updatePlayer :: Float -> Player -> [Char] -> Player
updatePlayer secs player@Player{playerPos = (x,y), playerSpeed, playerAnim} downKeys
  = player {playerPos = (clamp (x + mx) (-500,500), clamp (y + my) (-300,300)), playerAnim = animateR secs playerAnim} where
    (mx,my) = foldr checkKey (0,0) downKeys -- Move based on the keys currently being held down
    checkKey :: Char -> (Float, Float) -> (Float, Float)
    checkKey 's' (x,y) = (x, y - playerSpeed * secs)
    checkKey 'a' (x,y) = (x - playerSpeed * secs, y)
    checkKey 'w' (x,y) = (x, y + playerSpeed * secs)
    checkKey 'd' (x,y) = (x + playerSpeed * secs, y)
    checkKey _   acc   = acc

updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{deltaTime, playerBullets, explosions} = foldr shootPlayerBullet gstate{playerBullets = pbs} pbs where
  pbs = map (move deltaTime) playerBullets
  shootPlayerBullet :: PlayerBullet -> GameState -> GameState
  shootPlayerBullet pb gstate = case shoot pb ds of
    (Miss, _)      -> filterPlayerBullet pb gstate
    (Damage i, Just o@Obstacle{}) -> (destroy pb . update i o) gstate
    --(Damage i, Just x) -> undefined -- if x is an enemy: i is the index in the list ds, so convert i to index in list of enemies
    (Kill, Just o@Obstacle{obstaclePos}) -> (destroy pb . destroy o) gstate{explosions = newExplosion obstaclePos : explosions}
    (_, _)         -> gstate
  ds = obstacles gstate -- ++ enemies
  filterPlayerBullet :: PlayerBullet -> GameState -> GameState
  filterPlayerBullet pb gstate
    | x > 550 = destroy pb gstate
    | otherwise = gstate
    where (x,_) = pbPos pb

newExplosion :: Point -> Explosion
newExplosion pos = Explosion pos 0 (Animation 0 10 0.075 0)

updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions secs = mapMaybe animateExplosion where
  animateExplosion :: Explosion -> Maybe Explosion
  animateExplosion e@Explosion{explosionAnim} = case animateM secs explosionAnim of
    Nothing -> Nothing
    Just a  -> Just e{explosionAnim = a}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = fireBullet gstate
inputKey (EventKey (SpecialKey KeyEsc) Up _ _) gstate@GameState{paused} = gstate{paused = not paused}
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
friendlyBullet origin = PlayerBullet {
  pbPos    = origin,
  pbOrient = 0,
  pbDmg    = 10,
  pbSpeed  = 3000,
  pbHbox   = (10,2)
}

-- | Animation helper functions

-- Repeats animation when it reaches the end
animateR :: Float -> Animation -> Animation
animateR secs (Animation index total speed last)
  | last + secs > speed = Animation ((index + 1) `mod` total) total speed 0
  | otherwise = Animation index total speed (last + secs)

-- Returns Nothing when animation reaches the end
animateM :: Float -> Animation -> Maybe Animation
animateM secs (Animation index total speed last)
  | index + 1 > total   = Nothing
  | last + secs > speed = Just (Animation (index + 1) total speed 0)
  | otherwise           = Just (Animation index total speed (last + secs))