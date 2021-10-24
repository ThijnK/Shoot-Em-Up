{-# LANGUAGE NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Geometry.Line ( segClearsBox )
import Data.Maybe
import Data.List

data GameState = GameState {
  score         :: Int,
  paused        :: Bool,
  timeElapsed   :: Float,
  downKeys      :: [Char],
  player        :: Player,
  playerBullets :: [PlayerBullet],
  obstacles     :: [Obstacle],
  sprites       :: Sprites
}

initialState :: Sprites -> GameState
initialState sprites = GameState {
  score         = 0,
  paused        = False,
  timeElapsed   = 0.0,
  downKeys      = [],
  player        = Player (-100,0) 100 5 1 (13,8),
  playerBullets = [],
  obstacles     = [Obstacle (0,0) 50 (10,10)],
  sprites       = sprites
}

-- checkCollision (GameState 0 False 0.0 [] (Player (-100,0) 100 5 1 (13,8)) [(PlayerBullet (-100,0) 10 50 (10,2)), (PlayerBullet (0,0) 10 50 (20,50))] [(Obstacle (0,0) 100 (10,10))] (Sprites Blank Blank Blank)) [(PlayerBullet (0,0) 10 50 (10,2))]
-- test (GameState 0 False 0.0 [] (Player (-100,0) 100 5 1 (13,8)) [(PlayerBullet (-100,0) 10 50 (10,2)), (PlayerBullet (0,0) 10 50 (20,50))] [(Obstacle (0,0) 100 (10,10))] (Sprites Blank Blank Blank)) (PlayerBullet (-100,0) 10 50 (10,2))
-- GameState {score = 0, paused = False, timeElapsed = 0.0, downKeys = "", player = Player {playerPos = (-100.0,0.0), playerHp = 100, playerSpeed = 5.0, playerFr = 1.0, playerHbox = (13.0,8.0)}, playerBullets = [PlayerBullet {pbPos = (-100.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (10.0,2.0)},PlayerBullet {pbPos = (0.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (20.0,50.0)}], obstacles = [Obstacle {obstaclePos = (0.0,0.0), obstacleHp = 100, obstacleHbox = (10.0,10.0)}], sprites = Sprites {playerSprite = Blank, pBulletSprite = Blank, obstacleSprite = Blank}}
-- GameState {score = 0, paused = False, timeElapsed = 0.0, downKeys = "", player = Player {playerPos = (-100.0,0.0), playerHp = 100, playerSpeed = 5.0, playerFr = 1.0, playerHbox = (13.0,8.0)}, playerBullets = [PlayerBullet {pbPos = (-100.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (10.0,2.0)},PlayerBullet {pbPos = (0.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (20.0,50.0)}], obstacles = [], sprites = Sprites {playerSprite = Blank, pBulletSprite = Blank, obstacleSprite = Blank}}


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
  deriving Eq

data Obstacle = Obstacle {
  obstaclePos  :: Point,
  obstacleHp   :: Int,
  obstacleHbox :: Point
}
  deriving Eq

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
-- maybe instead: Bullet type class for bullets that check collision with respective things they can hit
class Collideable a where
  collide :: Collideable b => a -> b -> Bool
  collide a b = not (segClearsBox (xa - wa, ya - ha) (xa + wa, ya + ha) ll ur)
                || not (segClearsBox (xa - wa, ya + ha) (xa + wa, ya - ha) ll ur)
    where
      ((xa,ya), (wa,ha)) = getPosHitBox a
      ll = (xb - wb, yb - hb)
      ur = (xb + wb, yb + hb)
      ((xb,yb), (wb,hb)) = getPosHitBox b
  getPosHitBox :: a -> (Point, Point) -- TO DO: Maybe move this into seperate Object type class
  checkCollision :: GameState -> a -> GameState

instance Collideable a => Collideable [a] where
  getPosHitBox          = undefined
  checkCollision gstate = foldr (flip checkCollision) gstate

-- TO DO
instance Collideable Player where
  getPosHitBox p = (playerPos p, playerHbox p)
  checkCollision = undefined

instance Collideable PlayerBullet where
  getPosHitBox pb = (pbPos pb, pbHbox pb)
  checkCollision gstate@GameState{playerBullets, obstacles} x = case find (collide x) obstacles of
    Just o  -> obstacleHit gstate x o
    Nothing -> gstate

-- TO DO: move this to a more logical place
obstacleHit :: GameState -> PlayerBullet -> Obstacle -> GameState
obstacleHit gstate@GameState{playerBullets, obstacles} pb@PlayerBullet{pbDmg} o@Obstacle{obstacleHp}
  | newHp > 0 = gstate{playerBullets = delete pb playerBullets, obstacles = ((o{obstacleHp = newHp} :) . delete o) obstacles}
  | otherwise = gstate{playerBullets = delete pb playerBullets, obstacles = delete o obstacles}
  where newHp = obstacleHp - pbDmg

instance Collideable Obstacle where
  getPosHitBox o = (obstaclePos o, obstacleHbox o)
  checkCollision = undefined

-- | Moveable type class
class Moveable a where
  move :: GameState -> a -> Maybe a

instance Moveable a => Moveable [a] where
  move gstate = Just . mapMaybe (move gstate)

instance Moveable Player where
  move GameState{downKeys} player@Player{playerPos = (x,y), playerSpeed} = Just player {playerPos = (clamp (x + mx) (-500,500), clamp (y + my) (-300,300))} where
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

-- TO DO : move this to seperate file or something
clamp :: Float -> (Float, Float) -> Float
clamp x (l,u) = max (min x u) l