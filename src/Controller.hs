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
step secs gstate@GameState{player, playerBullets, downKeys, explosions} = return gstate' where
  gstate' = updatePlayerBullets gstate{player = p, playerBullets = pbs, explosions = e}
  p = movePlayer player downKeys
  pbs = map move playerBullets
  e = updateExplosions explosions

-- | Update objects
movePlayer :: Player -> [Char] -> Player
movePlayer player@Player{playerPos = (x,y), playerSpeed} downKeys = player {playerPos = (clamp (x + mx) (-500,500), clamp (y + my) (-300,300))} where
    (mx,my) = foldr checkKey (0,0) downKeys -- Move based on the keys currently being held down
    checkKey :: Char -> (Float, Float) -> (Float, Float)
    checkKey 's' (x,y) = (x, y - playerSpeed)
    checkKey 'a' (x,y) = (x - playerSpeed, y)
    checkKey 'w' (x,y) = (x, y + playerSpeed)
    checkKey 'd' (x,y) = (x + playerSpeed, y)
    checkKey _   acc   = acc

updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{playerBullets, explosions} = foldr shootPlayerBullet gstate playerBullets where
  shootPlayerBullet :: PlayerBullet -> GameState -> GameState
  shootPlayerBullet pb gstate = case shoot pb ds of
    (Miss, _)      -> gstate
    (Damage i, Just o@Obstacle{}) -> (destroy pb . update i o) gstate
    --(Damage i, Just x) -> undefined -- if x is an enemy: i is the index in the list ds, so convert i to index in list of enemies
    (Kill, Just o@Obstacle{obstaclePos}) -> (destroy pb . destroy o) gstate{explosions = Explosion obstaclePos 0 (0,10) : explosions}
    (_, _)         -> gstate
  ds = obstacles gstate -- ++ enemies

updateExplosions :: [Explosion] -> [Explosion]
updateExplosions = mapMaybe updateExplosion where
  updateExplosion :: Explosion -> Maybe Explosion
  updateExplosion e@Explosion{explosionAnim = (index, total)}
    | index + 1 > total = Nothing
    | otherwise = Just e{explosionAnim = (index + 1, total)}


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
