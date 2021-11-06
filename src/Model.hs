{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
  releasedKeys  :: [Char],
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
  explosionSprites :: [Picture]
} deriving Generic

data Explosion = Explosion {
  explosionPos    :: Point,
  explosionOrient :: Float,
  explosionAnim   :: Animation
} deriving Generic

data Player = Player {
  playerPos    :: Point,
  playerOrient :: Float,
  playerHp     :: Int,
  playerSpeed  :: Float,
  playerFr     :: FireRate,
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

-- Because everything is generic, we can just let aeson handle the encoding
instance ToJSON GameState

-- Help Aeson by instancing ToJSON for generics
instance ToJSON Score
instance ToJSON Player
instance ToJSON Turret
instance ToJSON Drone
instance ToJSON FireRate
instance ToJSON PlayerBullet
instance ToJSON EnemyBullet
instance ToJSON Meteor
instance ToJSON Explosion
instance ToJSON Animation
instance ToJSON Sprites where
  toJSON sprites = object []
instance ToJSON EnemyList
instance ToJSON EnemyListEnemy
instance ToJSON StdGen where
  toJSON stdgen = object []

-- Construct GameState from JSON
instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \v ->
    GameState
      <$> v .: "score"
      <*> v .: "paused"
      <*> v .: "gameOver"
      <*> v .: "deltaTime"
      <*> v .: "timeElapsed"
      <*> v .: "downKeys"
      <*> v .: "releasedKeys"
      <*> v .: "player"
      <*> v .: "turrets"
      <*> v .: "drones"
      <*> v .: "playerBullets"
      <*> v .: "enemyBullets"
      <*> v .: "meteors"
      <*> v .: "explosions"
      <*> v .: "sprites"
      <*> v .: "enemyList"
      <*> v .: "generator"

instance FromJSON Score
instance FromJSON Player
instance FromJSON FireRate
instance FromJSON Animation
instance FromJSON Turret
instance FromJSON Drone
instance FromJSON PlayerBullet
instance FromJSON EnemyBullet
instance FromJSON Meteor
instance FromJSON Explosion

instance FromJSON Sprites where
  parseJSON = withObject "Sprites" $ \obj -> do
    return (Sprites {playerSprites = [Blank], pBulletSprite = Blank, eBulletSprite = Blank, meteorSprite = Blank, turretSprites = [Blank], droneSprites = [Blank], kamikazeSprite = Blank, explosionSprites = [Blank]})
  
instance FromJSON StdGen where
  parseJSON = withObject "StdGen" $ \obj -> do
    return (mkStdGen 69)

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

