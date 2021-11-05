{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Classes
import Defaults

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Char
import Data.Maybe
import GHC.Float.RealFracMethods (int2Float)
import Graphics.Gloss.Data.Vector
import Control.Monad.State

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState{score, paused, gameOver, timeElapsed, player, meteors, downKeys, explosions}
  | gameOver  = return (order66 gstate')
  | paused    = return gstate -- No change to gamestate if game is paused
  | otherwise = return gstate'
  where
    gstate' = (spawnEnemies . updateTurrets . updateDrones . updateEnemyBullets . updatePlayerBullets)
      gstate {score = updateScore gameOver secs score, deltaTime = secs, timeElapsed = timeElapsed + secs, player = p, explosions = e, meteors = obs}
    p = updatePlayer secs player downKeys
    e = updateExplosions secs explosions
    obs = map (move secs) meteors

-- | Handle game over
order66 :: GameState -> GameState
order66 gstate@GameState{meteors, turrets, drones, explosions} = gstate{meteors = [], turrets = [], drones = [], explosions = es ++ explosions}
  where es = map defaultExplosion (map getPosition meteors ++ map getPosition turrets ++ map getPosition drones)

-- | Update score
updateScore :: Bool -> Float -> Score -> Score
updateScore True _ s = s
updateScore False secs s@(Score n incr last)
  | last + secs > incr = Score (n + 1) incr 0
  | otherwise = Score n incr (last + secs)

increaseScore :: Int -> Score -> Score
increaseScore x s@(Score n incr last) = Score (n + x) incr last

-- | Update player
updatePlayer :: Float -> Player -> [Char] -> Player
updatePlayer secs player@Player{playerSpeed, playerFr = FireRate fr last, playerAnim} downKeys
  = p{playerFr = FireRate fr (last + secs), playerAnim = animateR secs playerAnim} 
  where p = changePosition player (foldr (checkKey (playerSpeed * secs)) (0,0) downKeys) -- Move based on the keys currently being held down

checkKey :: Float -> Char -> (Float, Float) -> (Float, Float)
checkKey n 's' (x,y) = (x, y - n)
checkKey n 'a' (x,y) = (x - n, y)
checkKey n 'w' (x,y) = (x, y + n)
checkKey n 'd' (x,y) = (x + n, y)
checkKey _ _   acc   = acc

-- | Update player bullets
updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{deltaTime, playerBullets} = foldr shootPlayerBullet gstate{playerBullets = pbs} pbs where
  pbs = (filter withinBounds . map (move deltaTime)) playerBullets
  shootPlayerBullet :: PlayerBullet -> GameState -> GameState -- Check the collision of this bullet with meteors and all enemies
  shootPlayerBullet pb gstate@GameState{meteors, turrets, drones} = snd ((shootBullet meteors . shootBullet turrets . shootBullet drones) (Just pb, gstate))

-- | Update enemy bullets
updateEnemyBullets :: GameState -> GameState
updateEnemyBullets gstate@GameState{deltaTime, enemyBullets} = foldr shootEnemyBullet gstate{enemyBullets = ebs} ebs where
  ebs = (filter withinBounds . map (move deltaTime)) enemyBullets
  shootEnemyBullet :: EnemyBullet -> GameState -> GameState -- Check collision of this bullet with meteors and the player
  shootEnemyBullet eb gstate@GameState{player, meteors} = snd ((shootBullet meteors . shootBullet [player]) (Just eb, gstate))

-- | Update explosions
updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions secs = mapMaybe (\e@Explosion{explosionAnim} -> animateM secs explosionAnim >>= (\x -> return e{explosionAnim = x}))

-- | Update turrets
updateTurrets :: GameState -> GameState
updateTurrets gstate@GameState{deltaTime, player = Player{playerPos}, turrets, enemyBullets, generator}
  = gstate{turrets = (filter withinBounds . map (move deltaTime. updateTurret)) turrets, enemyBullets = foldr turretFire [] turrets ++ enemyBullets, generator = newGen} where
    updateTurret :: Turret -> Turret
    updateTurret t@Turret{turretFr, turretAnim} = t{turretFr = updateFr deltaTime turretFr, turretAnim = animateR deltaTime turretAnim}
    turretFire :: Turret -> [EnemyBullet] -> [EnemyBullet]
    turretFire Turret{turretPos, turretFr = FireRate fr last} acc
      | last + deltaTime > fr = defaultEnemyBullet turretPos (turretBulletOrient playerPos turretPos) : acc
      | otherwise = acc
    turretBulletOrient :: Point -> Point -> Float
    turretBulletOrient (px, py) (tx, ty) -- This function clamps the direction a turret can fire bullets to roughly in front of itself
      | angle > -0.75 * pi && angle < 0 = -0.75 * pi
      | angle < 0.75 * pi && angle > 0  = 0.75 * pi
      | otherwise                       = angle
      where angle = atan2 ((py + randVal)-ty) ((px + randVal)-tx)
    (randVal, newGen) = randomR (-20, 20) generator

-- | Update drones
updateDrones :: GameState -> GameState
updateDrones gstate@GameState{deltaTime, drones, enemyBullets}
  = gstate{drones = (filter withinBounds . map (move deltaTime. updateDrone)) drones, enemyBullets = foldr droneFire [] drones ++ enemyBullets} where
    updateDrone :: Drone -> Drone
    updateDrone d@Drone{droneFr, droneAnim} = d{droneFr = updateFr deltaTime droneFr, droneAnim = animateR deltaTime droneAnim}
    droneFire :: Drone -> [EnemyBullet] -> [EnemyBullet]
    droneFire Drone{dronePos, droneFr = FireRate fr last} acc
      | last + deltaTime > fr = droneBullets dronePos ++ acc
      | otherwise = acc
    droneBullets :: Point -> [EnemyBullet]
    droneBullets pos = map (defaultEnemyBullet pos) [0.75 * pi, 0.25 * pi, -0.25 * pi, -0.75 * pi, pi, 0, 0.5 * pi, -0.5 * pi]

-- | Spawn enemies based on the enemyList
spawnEnemies :: GameState -> GameState
spawnEnemies gstate@GameState{timeElapsed, enemyList, meteors, turrets, drones, generator}
  | timeElapsed > enemyTimer = addEnemy enemyType
  | otherwise = gstate
  where
    (enemyTimer, enemyType, newList) = enemyInfo (fst enemyList)
    addEnemy :: String -> GameState
    addEnemy "Turret" = gstate{enemyList = (newList, snd enemyList), turrets = defaultTurret (500, randYPos) randXPos : turrets, generator = newGen'}
    addEnemy "Drone"  = gstate{enemyList = (newList, snd enemyList), drones = defaultDrone (500, randYPos) : drones, generator = newGen}
    addEnemy "Meteor" = gstate{enemyList = (newList, snd enemyList), meteors = defaultMeteor (500, randYPos) : meteors, generator = newGen}
    addEnemy _        = gstate{enemyList = (newList, snd enemyList), generator = newGen}
    randYPos :: Float
    randYPos = int2Float iRandYPos
    randXPos = int2Float iRandXPos
    (iRandYPos, newGen) = randomR (-250, 250) generator
    (iRandXPos, newGen') = randomR (0, 450) generator

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
inputKey (EventKey (MouseButton LeftButton) Up _ _) gstate@GameState{gameOver, deltaTime, player, playerBullets}
  = let (p, pbs) = firePlayerBullet deltaTime gameOver player in gstate{player = p, playerBullets = pbs ++ playerBullets}
inputKey (EventKey (SpecialKey KeyEnter) Up _ _) gstate@GameState{gameOver, sprites, enemyList, generator}
  | gameOver = initialState sprites (snd enemyList) generator -- If the game is over and you press [Enter], you start over
  | otherwise = gstate
inputKey _ gstate = gstate

updateDownKeys :: KeyState -> Char -> GameState -> GameState
updateDownKeys Down c gstate@GameState{downKeys} = gstate { downKeys = c : downKeys }
updateDownKeys Up   c gstate@GameState{downKeys} = gstate { downKeys = delete c downKeys }

firePlayerBullet :: Float -> Bool -> Player -> (Player, [PlayerBullet])
firePlayerBullet secs False p@Player{playerPos, playerFr = FireRate fr last}
  | last + secs > fr = (p{playerFr = FireRate fr 0}, [defaultPlayerBullet playerPos])
  | otherwise = (p, [])
firePlayerBullet _ _ p = (p,[])

-- | General helper functions

-- Checks collision for a given bullet and a list of obstacles it can hit
shootBullet :: (Shootable a, Destructible a, Destructible b) => [b] -> (Maybe a, GameState) -> (Maybe a, GameState)
shootBullet ds (Just b, gstate@GameState{explosions}) = case shoot b ds of
  (Damage i, Just d) -> (Nothing, (destroy b . update i d) gstate)
  (Kill, Just d)     -> (Nothing, (checkIncreaseScore (shotByPlayer b) . destroy b . destroy d) gstate{explosions = defaultExplosion (getPosition d) : explosions})
  (_, _)             -> (Just b, gstate)
shootBullet _ x = x

checkIncreaseScore :: Bool -> GameState -> GameState
checkIncreaseScore True  gstate@GameState{score} = gstate{score = increaseScore 10 score}
checkIncreaseScore False gstate                  = gstate

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

-- Updates firerate
updateFr :: Float -> FireRate -> FireRate
updateFr secs (FireRate fr last)
  | last + secs > fr = FireRate fr 0
  | otherwise        = FireRate fr (last + secs)

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
