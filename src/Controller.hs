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
step secs gstate = return gstate {player = updatePlayer gstate, bullets = moveBullets (bullets gstate)}

updatePlayer :: GameState -> Player
updatePlayer gstate = Player sprite h (x + mx, y + my) s f hb where
  (mx, my) = calcMovement s (downKeys gstate) -- how much the x and y position of the player should change
  (Player sprite h (x,y) s f hb) = player gstate

moveBullets :: [Bullet] -> [Bullet]
moveBullets = mapMaybe f where
  f (Bullet sprite (x, y) dmg spd hbox t)
    | x < 550 = Just (Bullet sprite (x + spd, y) dmg spd hbox t)
    | otherwise = Nothing



-- Calculate the x and y movement based on the keys that are currently held down
calcMovement :: Float -> [Char] -> (Float, Float)
calcMovement s = foldr evaluateKey (0,0) where
  evaluateKey 'w' (x,y) = (x, y + s)
  evaluateKey 's' (x,y) = (x, y - s)
  evaluateKey 'a' (x,y) = (x - s, y)
  evaluateKey 'd' (x,y) = (x + s, y)
  evaluateKey _   acc   = acc

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
fireBullet gstate = gstate {bullets = friendlyBullet (bulletSprite gstate) origin : bullets gstate} where
  (Player _ _ origin _ _ _) = player gstate -- take the player's position as the origin of the bullet

friendlyBullet :: Picture -> Pos -> Bullet
friendlyBullet sprite origin = Bullet sprite origin 5 50 (5, 10) Friendly

enemyBullet :: Picture -> Pos -> Bullet
enemyBullet = undefined