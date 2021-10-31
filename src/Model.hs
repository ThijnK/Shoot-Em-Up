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
  explosions    :: [Explosion],
  sprites       :: Sprites
}

initialState :: Sprites -> GameState
initialState sprites = GameState {
  score         = 0,
  paused        = False,
  timeElapsed   = 0.0,
  downKeys      = [],
  player        = Player {
    playerPos = (-100, 0),
    playerOrient = 0,
    playerHp = 100,
    playerSpeed = 5,
    playerFr = 1,
    playerHbox = (13, 8)
  },
  playerBullets = [],
  obstacles     = [Obstacle (0,0) 0 50 (10,10)],
  explosions    = [Explosion (0,0) 0 (0, 10)],
  sprites       = sprites
}

-- checkCollision (GameState 0 False 0.0 [] (Player (-100,0) 100 5 1 (13,8)) [(PlayerBullet (-100,0) 10 50 (10,2)), (PlayerBullet (0,0) 10 50 (20,50))] [(Obstacle (0,0) 100 (10,10))] (Sprites Blank Blank Blank)) [(PlayerBullet (0,0) 10 50 (10,2))]
-- test (GameState 0 False 0.0 [] (Player (-100,0) 100 5 1 (13,8)) [(PlayerBullet (-100,0) 10 50 (10,2)), (PlayerBullet (0,0) 10 50 (20,50))] [(Obstacle (0,0) 100 (10,10))] (Sprites Blank Blank Blank)) (PlayerBullet (-100,0) 10 50 (10,2))
-- GameState {score = 0, paused = False, timeElapsed = 0.0, downKeys = "", player = Player {playerPos = (-100.0,0.0), playerHp = 100, playerSpeed = 5.0, playerFr = 1.0, playerHbox = (13.0,8.0)}, playerBullets = [PlayerBullet {pbPos = (-100.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (10.0,2.0)},PlayerBullet {pbPos = (0.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (20.0,50.0)}], obstacles = [Obstacle {obstaclePos = (0.0,0.0), obstacleHp = 100, obstacleHbox = (10.0,10.0)}], sprites = Sprites {playerSprite = Blank, pBulletSprite = Blank, obstacleSprite = Blank}}
-- GameState {score = 0, paused = False, timeElapsed = 0.0, downKeys = "", player = Player {playerPos = (-100.0,0.0), playerHp = 100, playerSpeed = 5.0, playerFr = 1.0, playerHbox = (13.0,8.0)}, playerBullets = [PlayerBullet {pbPos = (-100.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (10.0,2.0)},PlayerBullet {pbPos = (0.0,0.0), pbDmg = 10, pbSpeed = 50.0, pbHbox = (20.0,50.0)}], obstacles = [], sprites = Sprites {playerSprite = Blank, pBulletSprite = Blank, obstacleSprite = Blank}}


data Sprites  = Sprites {
  playerSprite     :: Picture,
  pBulletSprite    :: Picture,
  obstacleSprite   :: Picture,
  explosionSprites :: [Picture]
}

data Explosion = Explosion {
  explosionPos    :: Point,
  explosionOrient :: Float,
  explosionAnim   :: (Int, Int) -- = (currentSpriteIndex, totalSpriteCount)
}

data Player = Player {
  playerPos    :: Point,
  playerOrient :: Float,
  playerHp     :: Int,
  playerSpeed  :: Float,
  playerFr     :: Float,
  playerHbox   :: Point
} deriving Eq

data PlayerBullet = PlayerBullet {
  pbPos        :: Point,
  pbOrient     :: Float,
  pbDmg        :: Int,
  pbSpeed      :: Float,
  pbHbox       :: Point
} deriving Eq

-- data Enemy = Enemy {
--   enemyPos     :: Point,
--   enemyOrient :: Float,
--   enemyHp     :: Int,
--   enemySpeed  :: Float,
--   enemyFr     :: Float,
--   enemyHbox   :: Point
-- }

data Obstacle = Obstacle {
  obstaclePos    :: Point,
  obstacleOrient :: Float,
  obstacleHp     :: Int,
  obstacleHbox   :: Point
} deriving Eq

-- Enemy data type
-- Various bullet types
-- Power up data types
  -- data PowerUpType
  --   = FireRateIncrease
  --   | HealthIncrease
  --   | SpeedIncrease -- Increases movement speed
  --   | Invincibility


-- | Positionable type class
class Positionable a where
  getPosition :: a -> Point
  getOrientation :: a -> Float
  changePosition :: a -> Point -> a
  -- changeOrientation :: a -> Float -> a

instance Positionable Player where
  getPosition Player{playerPos} = playerPos
  getOrientation Player{playerOrient} = playerOrient
  changePosition p newPos = p{playerPos = newPos}

instance Positionable PlayerBullet where
  getPosition PlayerBullet{pbPos} = pbPos
  getOrientation PlayerBullet{pbOrient} = pbOrient
  changePosition pb newPos = pb{pbPos = newPos}

instance Positionable Obstacle where
  getPosition Obstacle{obstaclePos} = obstaclePos
  getOrientation Obstacle{obstacleOrient} = obstacleOrient
  changePosition o newPos = o{obstaclePos = newPos}

instance Positionable Explosion where
  getPosition Explosion{explosionPos} = explosionPos
  getOrientation Explosion{explosionOrient} = explosionOrient
  changePosition e newPos = e{explosionPos = newPos}

-- | Drawable type class
class Positionable a => Drawable a where
  getSprite :: Sprites -> a -> Picture
  toPicture :: Sprites -> a -> Picture
  toPicture sprites x = draw (getOrientation x) (getPosition x) (getSprite sprites x)

instance Drawable Player where
  getSprite Sprites{playerSprite} _ = playerSprite

instance Drawable PlayerBullet where
  getSprite Sprites{pBulletSprite} _ = pBulletSprite

instance Drawable Obstacle where
  getSprite Sprites{obstacleSprite} _ = obstacleSprite

instance Drawable Explosion where
  getSprite Sprites{explosionSprites} Explosion{explosionAnim = (index, _)} = explosionSprites !! index

draw :: Float -> Point -> Picture -> Picture
draw orientation (x,y) = rotate orientation . translate x y

-- | Collideable type class
class Collideable a where
  getHitbox :: a -> Point

instance Collideable Player where
  getHitbox Player{playerHbox} = playerHbox 

instance Collideable PlayerBullet where
  getHitbox PlayerBullet{pbHbox} = pbHbox

instance Collideable Obstacle where
  getHitbox Obstacle{obstacleHbox} = obstacleHbox

-- | Destructible type class
class (Positionable a, Collideable a, Eq a) => Destructible a where
  applyDamage :: a -> Int -> (Bool, a) -- For the Bool value: True means alive, False means dead
  destroy :: a -> GameState -> GameState
  update :: Int -> a -> GameState -> GameState

instance Destructible Player where
  applyDamage player@Player {playerHp} damage 
    | playerHp - damage <= 0 = (False, player{playerHp = playerHp - damage})
    | otherwise = (True, player{playerHp = playerHp - damage})
  destroy p gstate = undefined
  update _ p gstate = gstate{player = p}

instance Destructible Obstacle where
  applyDamage obs@Obstacle{obstacleHp} damage
    | obstacleHp - damage <= 0 = (False, obs)
    | otherwise = (True, obs {obstacleHp = obstacleHp - damage})
  destroy o gstate@GameState{obstacles} = gstate{obstacles = delete o obstacles}
  update i o gstate@GameState{obstacles} = gstate{obstacles = ((o :) . deleteAt i) obstacles}

instance Destructible PlayerBullet where
  applyDamage obs damage = undefined
  destroy pb gstate@GameState{playerBullets} = gstate{playerBullets = delete pb playerBullets}
  update i pb gstate@GameState{playerBullets} = gstate{playerBullets = ((pb :) . deleteAt i) playerBullets}

-- | Moveable type class
class Positionable a => Moveable a where
  getSpeed :: a -> Float
  move :: a -> a
  move a = changePosition a (clamp (x + (speed * cos orient)) (-500,500), clamp (y + (speed * sin orient)) (-300,300)) where
    (x,y) = getPosition a
    orient = getOrientation a / 180 * pi
    speed = getSpeed a

instance Moveable PlayerBullet where
  getSpeed PlayerBullet{pbSpeed} = pbSpeed

-- not sure about this
class (Positionable a, Collideable a) => Shootable a where
  shoot :: Destructible b => a -> [b] -> (HitInfo, Maybe b)

data HitInfo = Miss | Damage Int | Kill

instance Shootable PlayerBullet where
  shoot pb@PlayerBullet{pbDmg} xs = case find (collide pb) xs of
    Just x -> case applyDamage x pbDmg of
      (True, y)  -> (Damage i, Just y)
      (False, y) -> (Kill, Just y)
      where (Just i) = elemIndex x xs
    Nothing -> (Miss, Nothing)
    
    
{-    __        __
     /\ \      /\ \       [ ]    _   _     _______
    /  \ \    /  \ \       _    | | / /   |
   / /\ \ \  / /\ \ \     | |   | |/ /    |
  / / /\ \ \/ / /\ \ \    | |   | | /     |_______
 / / /  \_\/ / /  \_\ \   | |   | |\      |
/ / /    \_\/_/    \_\_\  | |   | | \     |
\/_/                \/_/  |_|   |_|  \_   |_______
-}

collide :: (Positionable a, Positionable b, Collideable a, Collideable b) => a -> b -> Bool
collide a b = not (segClearsBox (xa - wa, ya - ha) (xa + wa, ya + ha) ll ur)
              || not (segClearsBox (xa - wa, ya + ha) (xa + wa, ya - ha) ll ur)
  where
    (xa,ya) = getPosition a
    (wa,ha) = getHitbox a
    ll = (xb - wb, yb - hb)
    ur = (xb + wb, yb + hb)
    (xb,yb) = getPosition b
    (wb,hb) = getHitbox b

{-

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
    Just o  -> obstacleHit gstate{playerBullets = delete x playerBullets} (pbDmg x) o
    Nothing -> gstate

-- TO DO: move this to a more logical place
obstacleHit :: GameState -> Int -> Obstacle -> GameState
obstacleHit gstate@GameState{playerBullets, obstacles} dmg o@Obstacle{obstacleHp}
  | newHp > 0 = gstate{obstacles = ((o{obstacleHp = newHp} :) . delete o) obstacles}
  | otherwise = gstate{obstacles = delete o obstacles}
  where newHp = obstacleHp - dmg

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

-}

-- TO DO : move these helper functions to seperate file or something
clamp :: Float -> (Float, Float) -> Float
clamp x (l,u) = max (min x u) l

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = l ++ r
  where (l, _:r) = splitAt i xs