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
  activePUs     :: [PowerUpType], -- Currently active power ups
  turrets       :: [Turret],
  drones        :: [Drone],
  kamikazes     :: [Kamikaze],
  playerBullets :: [PlayerBullet],
  enemyBullets  :: [EnemyBullet],
  meteors       :: [Meteor],
  explosions    :: [Explosion],
  powerUps      :: [PowerUp],
  sprites       :: Sprites, -- Contains all used sprites
  spawnList     :: (SpawnList, SpawnList), -- Also stores a copy of the original spawnList
  bgList        :: [Background], -- List of background images
  generator     :: StdGen -- Random generator
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
  meteorSprite     :: Picture,
  turretSprites    :: [Picture],
  droneSprites     :: [Picture],
  kamikazeSprite   :: Picture,
  explosionSprites :: [Picture],
  backgroundSprites :: [Picture],
  hpPowerUp        :: Picture,
  speedPowerUp     :: Picture,
  frPowerUp        :: Picture,
  invincPowerUp    :: Picture
} deriving Generic

data Explosion = Explosion {
  explosionPos    :: Point,
  explosionOrient :: Float,
  explosionAnim   :: Animation,
  explosionSpeed  :: Float
} deriving Generic

data Player = Player {
  playerPos    :: Point,
  playerOrient :: Float,
  playerHp     :: (Int, Bool),
  playerSpeed  :: Float,
  playerFr     :: FireRate,
  playerHbox   :: Point,
  playerAnim   :: Animation
} deriving (Eq, Generic)

data Meteor = Meteor {
  meteorPos    :: Point,
  meteorOrient :: Float,
  meteorSpeed  :: Float,
  meteorHp     :: Int,
  meteorHbox   :: Point
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
} deriving (Show, Eq, Generic)

data EnemyBullet = EnemyBullet {
  ebPos    :: Point,
  ebOrient :: Float,
  ebDmg    :: Int,
  ebSpeed  :: Float,
  ebHbox   :: Point 
} deriving (Eq, Generic)

data Destructibles = PlayerDS [Meteor] [Turret] [Drone] [Kamikaze] -- Objects that can be destroyed by player bullets
                   | EnemyDS [Meteor] Player -- Objects that can be destroyed by enemy bullets

data Background = Background {
  backgroundXPos     :: Float,
  backgroundSpeed    :: Float,
  backgroundType     :: Int
} deriving (Eq, Generic)

data PowerUpType = Health Int | Speed Float Float | FR Float Float {-FireRate-} | Invincibility Float
  deriving (Eq, Generic)

instance Show PowerUpType where
  show (Speed _ t) = "Speed increased: " ++ take 4 (show t) ++ "s"
  show (FR _ t)    = "Fire rate increased: " ++ take 4 (show t) ++ "s"
  show (Invincibility t) = "Invincible: " ++ take 4 (show t) ++ "s"
  show _ = mempty

data PowerUp = PowerUp {
  puType   :: PowerUpType,
  puPos    :: Point,
  puOrient :: Float,
  puSpeed  :: Float,
  puHbox   :: Point
} deriving (Eq, Generic)

-- List used for spawning objects at given times
newtype SpawnList = SpawnList {objects :: [SpawnListItem]} 
  deriving (Show, Generic)

data SpawnListItem = SpawnListItem { 
  eleTime :: Float,
  eleType :: String
} deriving (Show, Generic)



{-    __        __
     /\ \      /\ \       [ ]    _   _     _______
    /  \ \    /  \ \       _    | | / /   |
   / /\ \ \  / /\ \ \     | |   | |/ /    |
  / / /\ \ \/ / /\ \ \    | |   | | /     |_______
 / / /  \ \/ / /  \_\ \   | |   | |\      |
/ / /    \_\/_/    \_\_\  | |   | | \     |
\/_/                \/_/  |_|   |_|  \_   |_______

┌────────────┐
└────┐  ┌────┘
     │  │     
     │  │     
     │  │ hijn    
     └──┘ 

-}