module Defaults where

import Model

import Graphics.Gloss ( Point )
import System.Random ( StdGen )

initialState :: Sprites -> EnemyList -> StdGen -> GameState
initialState sprites enemyList generator = GameState {
  score         = Score 0 2 0, -- Increase score by 1 every 2 seconds
  paused        = False,
  gameOver      = False,
  deltaTime     = 0.0,
  timeElapsed   = 0.0,
  downKeys      = [],
  releasedKeys  = [],
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
  enemyList     = (enemyList, enemyList),
  generator     = generator
}

defaultMeteor :: Point -> Meteor
defaultMeteor pos = Meteor pos 0 (-80) 50 (12, 12) -- Negative speed so they move to the left

defaultExplosion :: Point -> Explosion
defaultExplosion pos = Explosion pos 0 (Animation 0 10 0.075 0)

defaultTurret :: Point -> Float -> Turret
defaultTurret pos target = Turret pos 0 100 (-30) (FireRate 0.5 0) (8, 10) (Animation 0 4 0.2 0) target

defaultDrone :: Point -> Drone
defaultDrone pos = Drone pos 0 100 (-35) (FireRate 1 0) (8, 10) (Animation 0 4 0.2 0)

defaultPlayerBullet :: Point -> PlayerBullet
defaultPlayerBullet pos = PlayerBullet pos 0 10 1000 (10,2)

defaultEnemyBullet :: Point -> Float -> EnemyBullet
defaultEnemyBullet pos orient = EnemyBullet pos orient 5 666 (10,2)