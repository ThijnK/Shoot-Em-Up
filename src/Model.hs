{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Data.Maybe
import Data.List
import Data.Aeson
import GHC.Generics
import System.Random

data GameState = GameState {
  score         :: Score,
  paused        :: Bool,
  gameOver      :: Bool,
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
  enemyList     :: (EnemyList, EnemyList), -- Also stores a copy of the original enemylist
  generator     :: StdGen 
}

data Score = Score Int Float Float -- currentScore scoreIncrease(1 / scorePerSecond) secondsSinceLastIncrease

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
  kamikazeSprite   :: Picture,
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
  turretAnim   :: Animation,
  turretTarget :: Float -- Target that it will move towards
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

-- List used for spawning enemies at given times
newtype EnemyList = EnemyList {enemies :: [EnemyListEnemy]} 
  deriving (Show, Generic)
instance FromJSON EnemyList

data EnemyListEnemy = EnemyListEnemy
  { eleTime :: Float,
    eleType :: String
  }
  deriving (Show, Generic)
instance FromJSON EnemyListEnemy

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
-- data PowerUp = PowerUp {
--   -- powerUpType?
--   powerPos :: Point,
--   powerOrient :: Float,
--   powerHbox :: Point
-- }


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

-- TO DO : move these helper functions to seperate file or something
clamp :: Float -> (Float, Float) -> Float
clamp x (l,u) = max (min x u) l

replace :: Int -> a -> [a] -> [a]
replace index x xs = zs ++ (x:ys)
  where (zs, _:ys) = splitAt index xs

