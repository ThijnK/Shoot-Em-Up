-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Geometry.Line ( segClearsBox )

data GameState = GameState {
  score        :: Int,
  paused       :: Bool,
  timeElapsed  :: Float,
  player       :: Player,
  enemies      :: [Enemy],
  obstacles    :: [Obstacle],
  bullets      :: [Bullet],
  downKeys     :: [Char],
  bulletSprite :: Picture
}

initialState :: Sprites -> GameState
initialState (Sprites playerSprite bulletSprite) = GameState {
  score        = 0,
  paused       = False,
  timeElapsed  = 0.0,
  player       = Player playerSprite 100 (-100,0) 5 1 (13,8),
  enemies      = [],
  obstacles    = [],
  bullets      = [],
  downKeys     = [],
  bulletSprite = bulletSprite
}

type Health   = Int
type Pos      = Point -- x, y
type HitBox   = Point -- width, height
type FireRate = Float
type Speed    = Float
type Damage   = Int

data BulletType = Friendly | Hostile
data PowerUpType
  = FireRateIncrease
  | HealthIncrease
  | SpeedIncrease -- Increases movement speed
  | Invincibility
data Sprites  = Sprites Picture Picture

-- data Player = Player {
--   sprite   :: Picture,
--   pos      :: Point,
--   hp       :: Int,
--   speed    :: Float,
--   fireRate :: Float,
--   hitbox   :: Point
-- }
-- data Enemy = Enemy {
--   posE    :: Point,
--   hpE     :: Int,
--   hitboxE :: Point
-- }
-- data Obstacle = Obstacle {
--   posO :: Point,
--   hpO  :: Int,
--   hitboxO :: Point,
-- }
data Player   = Player Picture Health Pos Speed FireRate HitBox
data Enemy    = Enemy Health Pos HitBox
data Obstacle = Obstacle Health Pos HitBox
data Bullet   = Bullet Picture Pos Damage Speed HitBox BulletType
data PowerUp  = PowerUp PowerUpType Pos HitBox

-- | Drawable type class
class Drawable a where
  draw :: a -> Picture

instance Drawable Player where
  draw (Player sprite _ (x,y) _ _ _) = pictures [translate x y sprite, color red (line [(x-13,y-8),(x+13,y-8),(x+13,y+8),(x-13,y+8),(x-13,y-8)])]

instance Drawable Bullet where
  draw (Bullet sprite (x,y) _ _ _ _) = translate x y sprite

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
  getPosHitBox :: a -> (Pos, HitBox)

instance Collideable Player where
  getPosHitBox (Player _ _ p _ _ h) = (p,h)