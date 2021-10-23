{-# LANGUAGE NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Geometry.Line ( segClearsBox )
import Data.Maybe

data GameState = GameState {
  score         :: Int,
  paused        :: Bool,
  timeElapsed   :: Float,
  player        :: Player,
  obstacles     :: [Obstacle],
  playerBullets :: [PlayerBullet],
  downKeys      :: [Char],
  sprites       :: Sprites
}

initialState :: Sprites -> GameState
initialState sprites = GameState {
  score         = 0,
  paused        = False,
  timeElapsed   = 0.0,
  player        = Player (-100,0) 100 5 1 (13,8),
  obstacles     = [],
  playerBullets = [],
  downKeys      = [],
  sprites       = sprites
}

data Sprites  = Sprites {
  playerSprite   :: Picture,
  pBulletSprite  :: Picture,
  obstacleSprite :: Picture
}

data Player = Player {
  playerPos      :: Point,
  playerHp       :: Int,
  playerSpeed    :: Float,
  playerFr       :: Float,
  playerHbox     :: Point
}

data PlayerBullet = PlayerBullet {
  pbPos   :: Point,
  pbDmg   :: Int,
  pbSpeed :: Float,
  pbHbox  :: Point
}

data Obstacle = Obstacle {
  obstaclePos  :: Point,
  obstacleHp   :: Int,
  obstacleHbox :: Point
}

-- Enemy data type
-- Various bullet types
-- Power up data types
  -- data PowerUpType
  --   = FireRateIncrease
  --   | HealthIncrease
  --   | SpeedIncrease -- Increases movement speed
  --   | Invincibility

-- | Drawable type class
class Drawable a where
  draw :: Picture -> a -> Picture

instance Drawable Player where
  draw sprite p = translate x y sprite
    where (x,y) = playerPos p

instance Drawable PlayerBullet where
  draw sprite b = translate x y sprite
    where (x,y) = pbPos b

instance Drawable Obstacle where
  draw sprite o = translate x y sprite
    where (x,y) = obstaclePos o

-- | Collidable type class
class Collideable a where
  collide :: a -> a -> Bool
  collide a b = not (segClearsBox (xa - wa, ya - ha) (xa + wa, ya + ha) ll ur
                || segClearsBox (xa - wa, ya + ha) (xa + wa, ya - ha) ll ur)
    where
      ((xa,ya), (wa,ha)) = getPosHitBox a
      ll = (xb - wb, yb - hb)
      ur = (xb + wb, yb + hb)
      ((xb,yb), (wb,hb)) = getPosHitBox b
  getPosHitBox :: a -> (Point, Point)

instance Collideable Player where
  getPosHitBox p = (playerPos p, playerHbox p)

instance Collideable PlayerBullet where
  getPosHitBox pb = (pbPos pb, pbHbox pb)

instance Collideable Obstacle where
  getPosHitBox o = (obstaclePos o, obstacleHbox o)

-- | Moveable type class and instances
class Moveable a where
  move :: GameState -> a -> Maybe a

instance Moveable a => Moveable [a] where
  move gstate = Just . mapMaybe (move gstate)

instance Moveable Player where
  move GameState{downKeys} player@Player{playerPos = (x,y), playerSpeed} = Just player {playerPos = (x + mx, y + my)} where
    (mx,my) = foldr checkKey (0,0) downKeys -- Move based on the keys currently being held down
    checkKey :: Char -> (Float, Float) -> (Float, Float)
    checkKey 's' (x,y) = (x, y - playerSpeed)
    checkKey 'a' (x,y) = (x - playerSpeed, y)
    checkKey 'w' (x,y) = (x, y + playerSpeed)
    checkKey 'd' (x,y) = (x + playerSpeed, y)
    checkKey _   acc   = acc

instance Moveable PlayerBullet where
  move _ pb@PlayerBullet{pbPos = (x,y), pbSpeed}
    | nx < 550 = Just pb {pbPos = (nx, y)} -- Delete the bullet when it's off the screen
    | otherwise = Nothing
    where nx = x + pbSpeed