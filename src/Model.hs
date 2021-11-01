{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Geometry.Line ( segClearsBox )
import Data.Maybe
import Data.List
import Data.Aeson
import GHC.Generics
import System.Random

data GameState = GameState {
  score         :: Int,
  paused        :: Bool,
  deltaTime     :: Float,
  timeElapsed   :: Float,
  downKeys      :: [Char],
  player        :: Player,
  turrets       :: [Turret],
  drones        :: [Drone],
  playerBullets :: [PlayerBullet],
  enemyBullets  :: [EnemyBullet],
  obstacles     :: [Obstacle],
  explosions    :: [Explosion],
  sprites       :: Sprites,
  enemyList     :: EnemyList,
  generator     :: StdGen 
}

initialState :: Sprites -> EnemyList -> StdGen -> GameState
initialState sprites enemyList generator = GameState {
  score         = 0,
  paused        = False,
  deltaTime     = 0.0,
  timeElapsed   = 0.0,
  downKeys      = [],
  player        = Player {
    playerPos = (-100, 0),
    playerOrient = 0,
    playerHp = 100,
    playerSpeed = 300,
    playerFr = FireRate 0.2 0,
    playerHbox = (13, 8),
    playerAnim = Animation 0 8 0.2 0
  },
  turrets       = [],
  drones        = [],
  playerBullets = [],
  enemyBullets  = [],
  obstacles     = [defaultObstacle],
  explosions    = [],
  sprites       = sprites,
  enemyList     = enemyList,
  generator     = generator
}

defaultObstacle :: Obstacle
defaultObstacle = Obstacle (0, 0) pi 5 50 (10, 10)

data FireRate = FireRate Float Float -- fireRate(1 / bulletsPerSecond) secondsSinceLastShot
  deriving Eq
data Animation = Animation Int Int Float Float -- currentSpriteIndex totalSpriteCount animationSpeed(1 / fps) secondsSinceLastFrame
  deriving Eq

data Sprites  = Sprites {
  playerSprites    :: [Picture],
  pBulletSprite    :: Picture,
  eBulletSprite    :: Picture,
  obstacleSprite   :: Picture,
  turretSprites    :: [Picture],
  droneSprites     :: [Picture],
  explosionSprites :: [Picture]
}

data Explosion = Explosion {
  explosionPos    :: Point,
  explosionOrient :: Float,
  explosionAnim   :: Animation
}

data Player = Player {
  playerPos    :: Point,
  playerOrient :: Float,
  playerHp     :: Int,
  playerSpeed  :: Float,
  playerFr     :: FireRate,
  playerHbox   :: Point,
  playerAnim   :: Animation
} deriving Eq

data Turret = Turret {
  turretPos    :: Point,
  turretOrient :: Float,
  turretHp     :: Int,
  turretSpeed  :: Float,
  turretFr     :: FireRate,
  turretHbox   :: Point,
  turretAnim   :: Animation
} deriving Eq

data Drone = Drone {
  dronePos    :: Point,
  droneOrient :: Float,
  droneHp     :: Int,
  droneSpeed  :: Float,
  droneFr     :: FireRate,
  droneHbox   :: Point,
  droneAnim   :: Animation
} deriving Eq

data PlayerBullet = PlayerBullet {
  pbPos    :: Point,
  pbOrient :: Float,
  pbDmg    :: Int,
  pbSpeed  :: Float,
  pbHbox   :: Point
} deriving Eq

data EnemyBullet = EnemyBullet {
  ebPos    :: Point,
  ebOrient :: Float,
  ebDmg    :: Int,
  ebSpeed  :: Float,
  ebHbox   :: Point 
} deriving Eq

data Obstacle = Obstacle {
  obstaclePos    :: Point,
  obstacleOrient :: Float,
  obstacleSpeed  :: Float,
  obstacleHp     :: Int,
  obstacleHbox   :: Point
} deriving Eq

{-
Enemy ideas: 
- Turret = enemy that does not move and shoots bullets in player's direction
- Corvette (¬‿¬) = enemy that moves vertically towards the player and fires bullets straight in front of it
- Drone = moves with the player, spins around and fires multiple bullets around it at a time at fixed intervals
- Kamikaze = suicide bomber
-}

{-
Power up ideas: 
- Health
- FireRate
- Speed
- Invincibility
-}

-- Health Int | FireRate Int | Speed Int
data PowerUp = PowerUp {
  -- powerUpType?
  powerPos :: Point,
  powerOrient :: Float,
  powerHbox :: Point
}

-- | Positionable type class
class Positionable a where
  getPosition :: a -> Point
  getOrientation :: a -> Float
  changePosition :: a -> Point -> a
  -- changeOrientation :: a -> Float -> a

instance Positionable Player where
  getPosition Player{playerPos} = playerPos
  getOrientation Player{playerOrient} = playerOrient
  changePosition p@Player{playerPos = (x,y)} (mx,my) = p{playerPos = (x + mx, y + my)}

instance Positionable Turret where
  getPosition Turret{turretPos} = turretPos
  getOrientation Turret{turretOrient} = turretOrient
  changePosition t@Turret{turretPos = (x,y)} (mx,my) = t{turretPos = (x + mx, y + my)}

instance Positionable Drone where
  getPosition Drone{dronePos} = dronePos
  getOrientation Drone{droneOrient} = droneOrient
  changePosition d@Drone{dronePos = (x,y)} (mx,my) = d{dronePos = (x + mx, y + my)}

instance Positionable PlayerBullet where
  getPosition PlayerBullet{pbPos} = pbPos
  getOrientation PlayerBullet{pbOrient} = pbOrient
  changePosition pb@PlayerBullet{pbPos = (x,y)} (mx,my) = pb{pbPos = (x + mx, y + my)}

instance Positionable EnemyBullet where
  getPosition EnemyBullet{ebPos} = ebPos
  getOrientation EnemyBullet{ebOrient} = ebOrient
  changePosition eb@EnemyBullet{ebPos = (x,y)} (mx,my) = eb{ebPos = (x + mx, y + my)}

instance Positionable Obstacle where
  getPosition Obstacle{obstaclePos} = obstaclePos
  getOrientation Obstacle{obstacleOrient} = obstacleOrient
  changePosition o@Obstacle{obstaclePos = (x,y)} (mx,my) = o{obstaclePos = (x + mx, y + my)}

instance Positionable Explosion where
  getPosition Explosion{explosionPos} = explosionPos
  getOrientation Explosion{explosionOrient} = explosionOrient
  changePosition e@Explosion{explosionPos = (x,y)} (mx,my) = e{explosionPos = (x + mx, y + my)}

-- | Drawable type class
class Positionable a => Drawable a where
  getSprite :: Sprites -> a -> Picture
  toPicture :: Sprites -> a -> Picture
  toPicture sprites x = draw (getOrientation x) (getPosition x) (getSprite sprites x)

instance Drawable Player where
  getSprite Sprites{playerSprites} Player{playerAnim = Animation index _ _ _} = playerSprites !! index

instance Drawable Turret where
  getSprite Sprites{turretSprites} Turret{turretAnim = Animation index _ _ _} = turretSprites !! index

instance Drawable Drone where
  getSprite Sprites{droneSprites} Drone{droneAnim = Animation index _ _ _} = droneSprites !! index

instance Drawable PlayerBullet where
  getSprite Sprites{pBulletSprite} _ = pBulletSprite

instance Drawable EnemyBullet where
  getSprite Sprites{eBulletSprite} _ = eBulletSprite

instance Drawable Obstacle where
  getSprite Sprites{obstacleSprite} _ = obstacleSprite

instance Drawable Explosion where
  getSprite Sprites{explosionSprites} Explosion{explosionAnim = Animation index _ _ _} = explosionSprites !! index

draw :: Float -> Point -> Picture -> Picture
draw orientation (x,y) = rotate orientation . translate x y

-- | Collideable type class
class Collideable a where
  getHitbox :: a -> Point

instance Collideable Player where
  getHitbox Player{playerHbox} = playerHbox

instance Collideable Turret where
  getHitbox Turret{turretHbox} = turretHbox

instance Collideable Drone where
  getHitbox Drone{droneHbox} = droneHbox

instance Collideable PlayerBullet where
  getHitbox PlayerBullet{pbHbox} = pbHbox

instance Collideable EnemyBullet where
  getHitbox EnemyBullet{ebHbox} = ebHbox

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

instance Destructible Turret where
  applyDamage t@Turret{turretHp} damage
    | turretHp - damage <= 0 = (False, t)
    | otherwise = (True, t {turretHp = turretHp - damage})
  destroy t gstate@GameState{turrets} = gstate{turrets = delete t turrets}
  update i t gstate@GameState{turrets} = gstate{turrets = ((t :) . deleteAt i) turrets}

instance Destructible Drone where
  applyDamage d@Drone{droneHp} damage
    | droneHp - damage <= 0 = (False, d)
    | otherwise = (True, d {droneHp = droneHp - damage})
  destroy d gstate@GameState{drones} = gstate{drones = delete d drones}
  update i d gstate@GameState{drones} = gstate{drones = ((d :) . deleteAt i) drones}

instance Destructible Obstacle where
  applyDamage obs@Obstacle{obstacleHp} damage
    | obstacleHp - damage <= 0 = (False, obs)
    | otherwise = (True, obs {obstacleHp = obstacleHp - damage})
  destroy o gstate@GameState{obstacles} = gstate{obstacles = delete o obstacles}
  update i o gstate@GameState{obstacles} = gstate{obstacles = ((o :) . deleteAt i) obstacles}

instance Destructible PlayerBullet where
  applyDamage pb damage = undefined -- not used
  destroy pb gstate@GameState{playerBullets} = gstate{playerBullets = delete pb playerBullets}
  update i pb gstate@GameState{playerBullets} = gstate{playerBullets = ((pb :) . deleteAt i) playerBullets}

instance Destructible EnemyBullet where
  applyDamage eb damage = undefined
  destroy eb gstate@GameState{enemyBullets} = gstate{enemyBullets = delete eb enemyBullets}
  update i eb gstate@GameState{enemyBullets} = gstate{enemyBullets = ((eb :) . deleteAt i) enemyBullets}

-- | Moveable type class
class Positionable a => Moveable a where
  getSpeed :: a -> Float
  move :: Float -> a -> a
  move secs a = changePosition a (secs * speed * cos orient, secs * speed * sin orient) where
    orient = getOrientation a
    speed = getSpeed a

instance Moveable PlayerBullet where
  getSpeed PlayerBullet{pbSpeed} = pbSpeed

instance Moveable Obstacle where
  getSpeed Obstacle {obstacleSpeed} = obstacleSpeed

-- | Shootable type class
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
 / / /  \ \/ / /  \_\ \   | |   | |\      |
/ / /    \_\/_/    \_\_\  | |   | | \     |
\/_/                \/_/  |_|   |_|  \_   |_______

┌───────────┐
└────┐ ┌────┘
     │ │     
     │ │     
     │ │     
     └─┘ hijn

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

-- TO DO : move these helper functions to seperate file or something
clamp :: Float -> (Float, Float) -> Float
clamp x (l,u) = max (min x u) l

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = l ++ r
  where (l, _:r) = splitAt i xs



-- stuff for enemy spawning
-- hlint be angery when i put data so yea we have newtype now
newtype EnemyList = EnemyList {enemies :: [EnemyListEnemy]} deriving (Show, Generic)

data EnemyListEnemy = EnemyListEnemy
  { eleTime :: Float,
    eleType :: String
  }
  deriving (Show, Generic)

instance FromJSON EnemyList
instance FromJSON EnemyListEnemy