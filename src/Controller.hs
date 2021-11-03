{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Char
import Data.Maybe
import GHC.Float.RealFracMethods (int2Float)
import Graphics.Gloss.Data.Vector

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState{paused, gameOver, timeElapsed, player, meteors, downKeys, explosions}
  | gameOver  = return (order66 gstate')
  | paused    = return gstate -- No change to gamestate if game is paused
  | otherwise = return gstate'
  where
    gstate' = (spawnEnemies . updateTurrets . updateDrones . updateEnemyBullets . updatePlayerBullets) 
      gstate {deltaTime = secs, timeElapsed = timeElapsed + secs, player = p, explosions = e, meteors = obs}
    p = updatePlayer secs player downKeys
    e = updateExplosions secs explosions
    obs = map (move secs) meteors

-- | Handle game over
order66 :: GameState -> GameState
order66 gstate@GameState{meteors, turrets, drones, explosions} = gstate{meteors = [], turrets = [], drones = [], explosions = es ++ explosions}
  where es = map defaultExplosion (map getPosition meteors ++ map getPosition turrets ++ map getPosition drones)

-- | Update player
updatePlayer :: Float -> Player -> [Char] -> Player
updatePlayer secs player@Player{playerPos = (x,y), playerSpeed, playerFr = FireRate fr last, playerAnim} downKeys
  = player {playerPos = (clamp (x + mx) (-500,0), clamp (y + my) (-300,300)), playerFr = FireRate fr (last + secs), playerAnim = animateR secs playerAnim} where
    (mx,my) = foldr checkKey (0,0) downKeys -- Move based on the keys currently being held down
    checkKey :: Char -> (Float, Float) -> (Float, Float)
    checkKey 's' (x,y) = (x, y - playerSpeed * secs)
    checkKey 'a' (x,y) = (x - playerSpeed * secs, y)
    checkKey 'w' (x,y) = (x, y + playerSpeed * secs)
    checkKey 'd' (x,y) = (x + playerSpeed * secs, y)
    checkKey _   acc   = acc

-- | Update player bullets
updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{deltaTime, playerBullets} = foldr shootPlayerBullet gstate{playerBullets = pbs} pbs where
  pbs = map (move deltaTime) playerBullets
  shootPlayerBullet :: PlayerBullet -> GameState -> GameState
  shootPlayerBullet pb gstate@GameState{meteors, turrets, drones} = 
    case shootBullet pb gstate meteors of
      (True, gstate') -> case shootBullet pb gstate' turrets of
        (True, gstate'')  -> snd (shootBullet pb gstate'' drones)
        (False, gstate'') -> gstate''
      (False, gstate') -> gstate'

-- | Update enemy bullets
updateEnemyBullets :: GameState -> GameState
updateEnemyBullets gstate@GameState{deltaTime, enemyBullets} = foldr shootEnemyBullet gstate{enemyBullets = ebs} ebs where
  ebs = map (move deltaTime) enemyBullets
  shootEnemyBullet :: EnemyBullet -> GameState -> GameState
  shootEnemyBullet eb gstate@GameState{player, meteors} = 
    case shootBullet eb gstate [player] of
      (True, gstate') -> snd (shootBullet eb gstate' meteors)
      (False, gstate') -> gstate'

-- | Update explosions
updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions secs = mapMaybe animateExplosion where
  animateExplosion :: Explosion -> Maybe Explosion
  animateExplosion e@Explosion{explosionAnim} = case animateM secs explosionAnim of
    Nothing -> Nothing
    Just a  -> Just e{explosionAnim = a}

-- | Update turrets
updateTurrets :: GameState -> GameState
updateTurrets gstate@GameState{deltaTime, player = Player{playerPos}, turrets, enemyBullets, generator} 
  = gstate{turrets = (filter withinBounds . map updateTurret) turrets, enemyBullets = foldr turretFire [] turrets ++ enemyBullets, generator = newGen} where
    updateTurret :: Turret -> Turret
    updateTurret t@Turret{turretFr = FireRate fr last, turretAnim}
      | last + deltaTime > fr = move deltaTime t{turretFr = FireRate fr 0, turretAnim = animateR deltaTime turretAnim}
      | otherwise = move deltaTime t{turretFr = FireRate fr (last + deltaTime), turretAnim = animateR deltaTime turretAnim}
    turretFire :: Turret -> [EnemyBullet] -> [EnemyBullet]
    turretFire Turret{turretPos, turretFr = FireRate fr last} acc
      | last + deltaTime > fr = defaultEnemyBullet turretPos (turretBulletOrient playerPos turretPos) : acc
      | otherwise = acc
    turretBulletOrient :: Point -> Point -> Float
    turretBulletOrient (px, py) (tx, ty)
      | angle > -0.75 * pi && angle < 0 = -0.75 * pi
      | angle < 0.75 * pi && angle > 0  = 0.75 * pi
      | otherwise                       = angle
      where angle = atan2 ((py + randVal)-ty) ((px + randVal)-tx)
    (randVal, newGen) = randomR (-20, 20) generator

-- | Update drones
updateDrones :: GameState -> GameState
updateDrones gstate@GameState{deltaTime, drones, enemyBullets} 
  = gstate{drones = (filter withinBounds . map updateDrone) drones, enemyBullets = foldr droneFire [] drones ++ enemyBullets} where
    updateDrone :: Drone -> Drone
    updateDrone d@Drone{droneFr = FireRate fr last, droneAnim}
      | last + deltaTime > fr = move deltaTime d{droneFr = FireRate fr 0, droneAnim = animateR deltaTime droneAnim}
      | otherwise = move deltaTime d{droneFr = FireRate fr (last + deltaTime), droneAnim = animateR deltaTime droneAnim}
    droneFire :: Drone -> [EnemyBullet] -> [EnemyBullet]
    droneFire Drone{dronePos, droneFr = FireRate fr last} acc
      | last + deltaTime > fr = droneBullets dronePos ++ acc
      | otherwise = acc
    droneBullets :: Point -> [EnemyBullet]
    droneBullets pos = map (defaultEnemyBullet pos) [0.75 * pi, 0.25 * pi, -0.25 * pi, -0.75 * pi, pi, 0, 0.5 * pi, -0.5 * pi]

spawnEnemies :: GameState -> GameState --(EnemyList, [Meteor], [Turret], StdGen)
spawnEnemies gstate@GameState{timeElapsed, enemyList, meteors, turrets, drones, generator}
  | timeElapsed > enemyTimer = gstate'
  | otherwise = gstate
  where
    gstate' = gstate {enemyList = newList, meteors = newMeteor ++ meteors, turrets = newTurret ++ turrets, drones = newDrone ++ drones, generator = newGen}
    (enemyTimer, enemyType, newList) = enemyInfo enemyList
    -- Future proof :OOO
    -- newEnemy :: [Enemy]
    -- newEnemy
    --   | enemyType == "corvette" = []
    --   | otherwise = []
    newTurret   :: [Turret]
    newTurret | enemyType == "Turret" = [defaultTurret (500, randYPos)]
              | otherwise = []

    newDrone :: [Drone]
    newDrone
      | enemyType == "Drone" = [defaultDrone (500, randYPos)]
      | otherwise = []
    
    newMeteor :: [Meteor]
    newMeteor | enemyType == "Meteor" = [defaultMeteor (500, randYPos)]
                | otherwise = []

    range = (-250, 250)
    (iRandYPos, newGen) = randomR range generator
    randYPos = int2Float iRandYPos

-- Returns enemy info if at least one enemy has yet to be spawned
enemyInfo :: EnemyList -> (Float, String, EnemyList)
enemyInfo eList@(EnemyList (EnemyListEnemy enemyTimer enemyType : xs)) = (enemyTimer, enemyType, EnemyList xs)
enemyInfo eList@(EnemyList []) = (1.0 / 0, "", eList)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyEsc) Up _ _) gstate@GameState{paused} = gstate{paused = not paused}
inputKey (EventKey (Char c) d _ _) gstate
  | c == 'w' || c == 'a' || c == 's' || c == 'd' = updateDownKeys d c gstate
  | otherwise = gstate
inputKey (EventKey (MouseButton LeftButton) Up _ _) gstate@GameState{deltaTime, player, playerBullets} 
  = let (p, pbs) = firePlayer deltaTime player in gstate{player = p, playerBullets = pbs ++ playerBullets}
inputKey (EventKey (SpecialKey KeyEnter) Up _ _) gstate@GameState{gameOver, sprites, enemyList, generator}
  | gameOver = initialState sprites enemyList generator -- If the game is over and you press [Enter], you start over
  | otherwise = gstate
inputKey _ gstate = gstate

updateDownKeys :: KeyState -> Char -> GameState -> GameState
updateDownKeys Down c gstate@GameState{downKeys} = gstate { downKeys = c : downKeys }
updateDownKeys Up   c gstate@GameState{downKeys} = gstate { downKeys = delete c downKeys }

firePlayer :: Float -> Player -> (Player, [PlayerBullet])
firePlayer secs p@Player{playerPos, playerFr = FireRate fr last}
  | last + secs > fr = (p{playerFr = FireRate fr 0}, [defaultPlayerBullet playerPos])
  | otherwise = (p, [])


-- | General helper functions

-- Checks collision for a given bullet and a list of obstacles it can hit
shootBullet :: (Shootable a, Destructible a, Destructible b) => a -> GameState -> [b] -> (Bool, GameState)
shootBullet b gstate@GameState{explosions} ds = case shoot b ds of
  (Miss, _) -> filter' b gstate
  (Damage i, Just d) -> (False, (destroy b . update i d) gstate)
  (Kill, Just d) -> (False, (destroy b . destroy d) gstate{explosions = defaultExplosion (getPosition d) : explosions})
  (_, _) -> (True, gstate)

-- Replace the element at the given index of the list with the given value
replace :: Int -> a -> [a] -> [a]
replace index x xs = zs ++ (x:ys)
  where (zs, _:ys) = splitAt index xs

-- Remove an object from gamestate if it's outside of bounds
filter' :: Destructible a => a -> GameState -> (Bool, GameState)
filter' a gstate
  | not (withinBounds a) = (False, destroy a gstate)
  | otherwise = (True, gstate)
  where (x,_) = getPosition a

withinBounds :: Positionable a => a -> Bool
withinBounds a
  | x > 550 || x < -550 = False
  | otherwise = True
  where (x,_) = getPosition a

-- Repeats animation when it reaches the end
animateR :: Float -> Animation -> Animation
animateR secs (Animation index total speed last)
  | last + secs > speed = Animation ((index + 1) `mod` total) total speed 0
  | otherwise = Animation index total speed (last + secs)

-- Returns Nothing when animation reaches the end
animateM :: Float -> Animation -> Maybe Animation
animateM secs (Animation index total speed last)
  | index + 1 > total   = Nothing
  | last + secs > speed = Just (Animation (index + 1) total speed 0)
  | otherwise           = Just (Animation index total speed (last + secs))