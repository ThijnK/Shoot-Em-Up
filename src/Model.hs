-- | This module contains the data types
--   which represent the state of the game
module Model where

newtype World = World {
  ship :: Position
}

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
  world       :: World,
  score       :: Int,
  paused      :: Bool,
  timeElapsed :: Float,
  player      :: Player,
  enemies     :: [Enemy],
  obstacles   :: [Obstacle],
  bullets     :: [Bullet]
}

initialState :: GameState
initialState =
    GameState
        { 
            world = World { ship = (50,0)},
            score = 0,
            paused = False,
            timeElapsed = 0.0,
            player = undefined,
            enemies = [],
            obstacles = [],
            bullets = []
        }

type Health = Int

type Position = (Float, Float) -- x, y

type HitBox = (Int, Int) -- width, height

type FireRate = Float

type Speed = Float

type Damage = Int

data BulletType = Friendly | Hostile

data PowerUpType
  = FireRateIncrease
  | HealthIncrease
  | SpeedIncrease -- Increases vertical movement speed
  | Invincibility

data Player = Player Health Position Speed FireRate HitBox

data Enemy = Enemy Health Position HitBox

data Obstacle = Obstacle Health Damage Position HitBox

data Bullet = Bullet Position Damage Speed HitBox BulletType

data PowerUp = PowerUp PowerUpType Position HitBox

playerMove :: Int -> GameState -> GameState -- moves the player
playerShoot :: GameState -> GameState -- spawns a bullet
playerMove = undefined

playerShoot = undefined

test :: GameState -> GameState
test gstate = gstate {score = score gstate + 1}