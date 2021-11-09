{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss ( Picture, Point )
import GHC.Generics ( Generic )
import System.Random ( StdGen )

data GameState = GameState {
  score         :: Score,
  paused        :: Bool,
  gameOver      :: Bool,
  deltaTime     :: Float,
  timeElapsed   :: Float,
  downKeys      :: [Char],
  saveLoad      :: (Bool, Bool), -- (wantsToSave, wantsToLoad)
  player        :: Player,
  turrets       :: [Turret],
  drones        :: [Drone],
  kamikazes     :: [Kamikaze],
  playerBullets :: [PlayerBullet],
  enemyBullets  :: [EnemyBullet],
  meteors       :: [Meteor],
  explosions    :: [Explosion],
  powerUps      :: [PowerUp],
  sprites       :: Sprites,
  enemyList     :: (EnemyList, EnemyList), -- Also stores a copy of the original enemylist
  generator     :: StdGen
} deriving Generic

data Score = Score Int Float Float -- currentScore scoreIncrease(1 / scorePerSecond) secondsSinceLastIncrease
  deriving Generic 

data FireRate = FireRate Float Float -- fireRate(1 / bulletsPerSecond) secondsSinceLastShot
  deriving (Eq, Generic)
data Animation = Animation Int Int Float Float -- currentSpriteIndex totalSpriteCount animationSpeed(1 / fps) secondsSinceLastFrame
  deriving (Eq, Generic)

data Sprites  = Sprites {
  playerSprites    :: [Picture],
  pBulletSprite    :: Picture,
  eBulletSprite    :: Picture,
  meteorSprite   :: Picture,
  turretSprites    :: [Picture],
  droneSprites     :: [Picture],
  kamikazeSprite   :: Picture,
  explosionSprites :: [Picture],
  hpPowerUp        :: Picture,
  speedPowerUp     :: Picture,
  frPowerUp        :: Picture,
  invincPowerUp    :: Picture
} deriving Generic

data Explosion = Explosion {
  explosionPos    :: Point,
  explosionOrient :: Float,
  explosionAnim   :: Animation
} deriving Generic

data Player = Player {
  playerPos    :: Point,
  playerOrient :: Float,
  playerHp     :: (Int, PowerUpType),
  playerSpeed  :: (Float, PowerUpType),
  playerFr     :: (FireRate, PowerUpType),
  playerHbox   :: Point,
  playerAnim   :: Animation
} deriving (Eq, Generic)

data Turret = Turret {
  turretPos    :: Point,
  turretOrient :: Float,
  turretHp     :: Int,
  turretSpeed  :: Float,
  turretFr     :: FireRate,
  turretHbox   :: Point,
  turretAnim   :: Animation,
  turretTarget :: Float -- Target that it will move towards
} deriving (Eq, Generic)

data Drone = Drone {
  dronePos    :: Point,
  droneOrient :: Float,
  droneHp     :: Int,
  droneSpeed  :: Float,
  droneFr     :: FireRate,
  droneHbox   :: Point,
  droneAnim   :: Animation
} deriving (Eq, Generic)

data Kamikaze = Kamikaze {
  kamikazePos    :: Point,
  kamikazeOrient :: Float,
  kamikazeHp     :: Int,
  kamikazeSpeed  :: Float,
  kamikazeHbox   :: Point
} deriving (Eq, Generic)

data PlayerBullet = PlayerBullet {
  pbPos    :: Point,
  pbOrient :: Float,
  pbDmg    :: Int,
  pbSpeed  :: Float,
  pbHbox   :: Point
} deriving (Eq, Generic)

data EnemyBullet = EnemyBullet {
  ebPos    :: Point,
  ebOrient :: Float,
  ebDmg    :: Int,
  ebSpeed  :: Float,
  ebHbox   :: Point 
} deriving (Eq, Generic)

data Meteor = Meteor {
  meteorPos    :: Point,
  meteorOrient :: Float,
  meteorSpeed  :: Float,
  meteorHp     :: Int,
  meteorHbox   :: Point
} deriving (Eq, Generic)

data PowerUpType = Health Int | Speed Float Float | FR Float Float {-FireRate-} | Invincibility Float
  deriving (Eq, Generic)
data PowerUp = PowerUp {
  puType   :: PowerUpType,
  puPos    :: Point,
  puOrient :: Float,
  puSpeed  :: Float,
  puHbox   :: Point
} deriving (Eq, Generic)

-- List used for spawning enemies at given times
newtype EnemyList = EnemyList {enemies :: [EnemyListEnemy]} 
  deriving (Show, Generic)

data EnemyListEnemy = EnemyListEnemy
  { eleTime :: Float,
    eleType :: String
  }
  deriving (Show, Generic)

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