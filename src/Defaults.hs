-- Contains functions to construct all default instances of the various objects in the game
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
  saveLoad      = (False, False),
  player        = Player {
    playerPos = (-100, 0),
    playerOrient = 0,
    playerHp = 100,
    playerSpeed = 300,
    playerFr = FireRate 0.10 0,
    playerHbox = (18, 8),
    playerAnim = Animation 0 8 0.2 0
  },
  turrets       = [],
  drones        = [],
  kamikazes     = [],
  playerBullets = [],
  enemyBullets  = [],
  meteors       = [],
  explosions    = [],
  sprites       = sprites,
  enemyList     = (enemyList, enemyList),
  bgList        = [Background 0 (-100.0) 0,  Background 1024 (-100.0) 0, Background 512 (-200.0) 1, Background 1537 (-200.0) 1],
  generator     = generator
}

defaultMeteor :: Point -> Float -> Meteor
defaultMeteor pos speed = Meteor pos 0 speed 100 (12, 12) -- Negative speed so they move to the left

defaultExplosion :: Point -> Explosion
defaultExplosion pos = Explosion pos 0 (Animation 0 10 0.075 0)

defaultTurret :: Point -> Float -> Turret
defaultTurret pos target = Turret pos 0 100 (-30) (FireRate 1.5 0) (8, 10) (Animation 0 4 0.2 0) target

defaultDrone :: Point -> Drone
defaultDrone pos = Drone pos 0 125 (-35) (FireRate 1 0) (8, 10) (Animation 0 4 0.2 0)

defaultKamikaze :: Point -> Kamikaze 
defaultKamikaze pos = Kamikaze pos 0 50 200 (13, 6)

defaultPlayerBullet :: Point -> PlayerBullet
defaultPlayerBullet pos = PlayerBullet pos 0 25 1000 (10,2)

defaultEnemyBullet :: Point -> Float -> EnemyBullet
defaultEnemyBullet pos orient = EnemyBullet pos orient 5 500 (10,2)