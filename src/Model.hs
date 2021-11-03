{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Geometry.Line ( segClearsBox )
import Graphics.Gloss.Geometry.Angle (radToDeg)
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
  meteors       :: [Meteor],
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
    playerFr = FireRate 0.10 0,
    playerHbox = (13, 8),
    playerAnim = Animation 0 8 0.2 0
  },
  turrets       = [],
  drones        = [],
  playerBullets = [],
  enemyBullets  = [],
  meteors     = [defaultMeteor (0,200)],
  explosions    = [],
  sprites       = sprites,
  enemyList     = enemyList,
  generator     = generator
}

defaultMeteor :: Point -> Meteor
defaultMeteor pos = Meteor pos 0 (-80) 50 (12, 12) -- Negative speed so they move to the left

defaultExplosion :: Point -> Explosion
defaultExplosion pos = Explosion pos 0 (Animation 0 10 0.075 0)

defaultTurret :: Point -> Turret
defaultTurret pos = Turret pos 0 100 (-30) (FireRate 0.5 0) (8, 10) (Animation 0 4 0.2 0)

defaultDrone :: Point -> Drone
defaultDrone pos = Drone pos 0 100 (-35) (FireRate 1 0) (8, 10) (Animation 0 4 0.2 0)

defaultPlayerBullet :: Point -> PlayerBullet
defaultPlayerBullet pos = PlayerBullet pos 0 10 1000 (10,2)

defaultEnemyBullet :: Point -> Float -> EnemyBullet
defaultEnemyBullet pos orient = EnemyBullet pos orient 5 666 (10,2)

data FireRate = FireRate Float Float -- fireRate(1 / bulletsPerSecond) secondsSinceLastShot
  deriving Eq
data Animation = Animation Int Int Float Float -- currentSpriteIndex totalSpriteCount animationSpeed(1 / fps) secondsSinceLastFrame
  deriving Eq

data Sprites  = Sprites {
  playerSprites    :: [Picture],
  pBulletSprite    :: Picture,
  eBulletSprite    :: Picture,
  meteorSprite   :: Picture,
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

data Meteor = Meteor {
  meteorPos    :: Point,
  meteorOrient :: Float,
  meteorSpeed  :: Float,
  meteorHp     :: Int,
  meteorHbox   :: Point
} deriving Eq

{-
Enemy ideas: 
- Turret = enemy that does not move and shoots bullets in player's direction
- Corvette = enemy that moves vertically towards the player and fires bullets straight in front of it
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

instance Positionable Meteor where
  getPosition Meteor{meteorPos} = meteorPos
  getOrientation Meteor{meteorOrient} = meteorOrient
  changePosition o@Meteor{meteorPos = (x,y)} (mx,my) = o{meteorPos = (x + mx, y + my)}

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

instance Drawable Meteor where
  getSprite Sprites{meteorSprite} _ = meteorSprite

instance Drawable Explosion where
  getSprite Sprites{explosionSprites} Explosion{explosionAnim = Animation index _ _ _} = explosionSprites !! index

draw :: Float -> Point -> Picture -> Picture
draw orientation (x,y) = translate x y . rotate (radToDeg (-orientation)) -- credits to student no. 5923402482 (J. S. C. Lee)

drawHbox :: (Positionable a, Collideable a) => a -> Picture
drawHbox a = color red (line [(x-w,y-h),(x+w,y-h),(x+w,y+h),(x-w,y+h),(x-w,y-h)]) where
    (x,y) = getPosition a
    (w,h) = getHitbox a

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

instance Collideable Meteor where
  getHitbox Meteor{meteorHbox} = meteorHbox

-- | Destructible type class
class (Positionable a, Collideable a, Eq a) => Destructible a where
  applyDamage :: a -> Int -> (Bool, a) -- For the Bool value: True means alive, False means dead
  destroy :: a -> GameState -> GameState
  update :: Int -> a -> GameState -> GameState

instance Destructible Player where
  applyDamage player@Player {playerHp} damage
    | playerHp - damage <= 0 = (False, player{playerHp = playerHp - damage})
    | otherwise = (True, player{playerHp = playerHp - damage})
  destroy p gstate = gstate{paused = True} -- TO DO: make this indicate game over
  update _ p gstate = gstate{player = p}

instance Destructible Turret where
  applyDamage t@Turret{turretHp} damage
    | turretHp - damage <= 0 = (False, t)
    | otherwise = (True, t {turretHp = turretHp - damage})
  destroy t gstate@GameState{turrets} = gstate{turrets = delete t turrets}
  update i t gstate@GameState{turrets} = gstate{turrets = replace i t turrets}

instance Destructible Drone where
  applyDamage d@Drone{droneHp} damage
    | droneHp - damage <= 0 = (False, d)
    | otherwise = (True, d {droneHp = droneHp - damage})
  destroy d gstate@GameState{drones} = gstate{drones = delete d drones}
  update i d gstate@GameState{drones} = gstate{drones = replace i d drones}

instance Destructible Meteor where
  applyDamage obs@Meteor{meteorHp} damage
    | meteorHp - damage <= 0 = (False, obs)
    | otherwise = (True, obs {meteorHp = meteorHp - damage})
  destroy o gstate@GameState{meteors} = gstate{meteors = delete o meteors}
  update i o gstate@GameState{meteors} = gstate{meteors = replace i o meteors}

instance Destructible PlayerBullet where
  applyDamage pb damage = undefined -- not used
  destroy pb gstate@GameState{playerBullets} = gstate{playerBullets = delete pb playerBullets}
  update i pb gstate@GameState{playerBullets} = gstate{playerBullets = replace i pb playerBullets}

instance Destructible EnemyBullet where
  applyDamage eb damage = undefined
  destroy eb gstate@GameState{enemyBullets} = gstate{enemyBullets = delete eb enemyBullets}
  update i eb gstate@GameState{enemyBullets} = gstate{enemyBullets = replace i eb enemyBullets}

-- | Moveable type class
class Positionable a => Moveable a where
  getSpeed :: a -> Float
  move :: Float -> a -> a
  move secs a = changePosition a (secs * speed * cos orient, secs * speed * sin orient) where
    orient = getOrientation a
    speed = getSpeed a

instance Moveable PlayerBullet where
  getSpeed PlayerBullet{pbSpeed} = pbSpeed

instance Moveable Meteor where
  getSpeed Meteor {meteorSpeed} = meteorSpeed

instance Moveable Turret where
  getSpeed Turret{turretSpeed} = turretSpeed

instance Moveable Drone where
  getSpeed Drone{droneSpeed} = droneSpeed

instance Moveable EnemyBullet where
  getSpeed EnemyBullet{ebSpeed} = ebSpeed

-- | Shootable type class
class (Positionable a, Collideable a) => Shootable a where
  getDmg :: a -> Int
  shoot :: Destructible b => a -> [b] -> (HitInfo, Maybe b)
  shoot b xs = case find (collide b) xs of
    Just x -> case applyDamage x (getDmg b) of
      (True, y)  -> (Damage i, Just y)
      (False, y) -> (Kill, Just y)
      where (Just i) = elemIndex x xs
    Nothing -> (Miss, Nothing)

data HitInfo = Miss | Damage Int | Kill

instance Shootable PlayerBullet where
  getDmg PlayerBullet{pbDmg} = pbDmg

instance Shootable EnemyBullet where
  getDmg EnemyBullet{ebDmg} = ebDmg


{-    __        __
     /\ \      /\ \       [ ]    _   _     _______
    /  \ \    /  \ \       _    | | / /   |
   / /\ \ \  / /\ \ \     | |   | |/ /    |
  / / /\ \ \/ / /\ \ \    | |   | | /     |_______
 / / /  \ \/ / /  \_\ \   | |   | |\      |
/ / /    \_\/_/    \_\_\  | |   | | \     |
\/_/                \/_/  |_|   |_|  \_   |_______

┌───────────┐
└────┐  ┌────┘
     │  │     
     │  │     
     │  │     
     └─ ┘ hijn

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

replace :: Int -> a -> [a] -> [a]
replace index x xs = zs ++ (x:ys)
  where (zs, _:ys) = splitAt index xs

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