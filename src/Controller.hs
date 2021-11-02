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

-- secs * speed to normalize

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@GameState{paused, timeElapsed, player, turrets, enemyBullets, downKeys, explosions}
  | paused = return gstate -- No change to gamestate if game is paused
  | otherwise = return gstate'
  where
    gstate' = (updateTurrets . updatePlayerBullets) gstate {deltaTime = secs, timeElapsed = t, player = p, enemyBullets = ebs, explosions = e, obstacles = obs, enemyList = eList, generator = newGen}
    t = timeElapsed + secs
    p = updatePlayer secs player downKeys
    e = updateExplosions secs explosions
    ebs = map (move secs) enemyBullets
    obs = map (move secs) obs'
    (eList@(EnemyList enemies), obs', newGen) = spawnEnemies gstate

-- | Update player
updatePlayer :: Float -> Player -> [Char] -> Player
updatePlayer secs player@Player{playerPos = (x,y), playerSpeed, playerAnim} downKeys
  = player {playerPos = (clamp (x + mx) (-500,500), clamp (y + my) (-300,300)), playerAnim = animateR secs playerAnim} where
    (mx,my) = foldr checkKey (0,0) downKeys -- Move based on the keys currently being held down
    checkKey :: Char -> (Float, Float) -> (Float, Float)
    checkKey 's' (x,y) = (x, y - playerSpeed * secs)
    checkKey 'a' (x,y) = (x - playerSpeed * secs, y)
    checkKey 'w' (x,y) = (x, y + playerSpeed * secs)
    checkKey 'd' (x,y) = (x + playerSpeed * secs, y)
    checkKey _   acc   = acc



-- | Update player bullets
updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{deltaTime, playerBullets, explosions} = foldr shootPlayerBullet gstate{playerBullets = pbs} pbs where
  pbs = map (move deltaTime) playerBullets
  shootPlayerBullet :: PlayerBullet -> GameState -> GameState
  shootPlayerBullet pb gstate@GameState{obstacles, turrets, drones} = case shootBullet pb gstate obstacles of
    (True, gstate') -> snd (shootBullet pb gstate' turrets)
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
updateTurrets gstate@GameState{deltaTime, player = Player{playerPos = (px,py)}, turrets, enemyBullets} 
  = gstate{turrets = map updateTurret turrets, enemyBullets = foldr turretFire [] turrets ++ enemyBullets} where
    updateTurret :: Turret -> Turret
    updateTurret t@Turret{turretFr = FireRate fr last, turretAnim}
      | last + deltaTime > fr = move deltaTime t{turretFr = FireRate fr 0, turretAnim = animateR deltaTime turretAnim}
      | otherwise = move deltaTime t{turretFr = FireRate fr (last + deltaTime), turretAnim = animateR deltaTime turretAnim}
    turretFire :: Turret -> [EnemyBullet] -> [EnemyBullet]
    turretFire Turret{turretPos = tp@(tx,ty), turretFr = FireRate fr last} acc
      | last + deltaTime > fr = EnemyBullet tp (argV (px - tx, py - ty)) 10 3000 (10,2): acc
      | otherwise = acc

spawnEnemies :: GameState -> (EnemyList, [Obstacle], StdGen)
spawnEnemies gstate@GameState{timeElapsed, enemyList, obstacles, generator}
  | timeElapsed > enemyTimer = (newList, newObstacle ++ obstacles, newGen)
  | otherwise = (enemyList, obstacles, generator)
  where
    (enemyTimer, enemyType, newList) = enemyInfo enemyList
    -- Future proof :OOO
    -- newEnemy :: [Enemy]
    -- newEnemy
    --   | enemyType == "corvette" = []
    --   | otherwise = []
    newObstacle :: [Obstacle]
    newObstacle | enemyType == "Obstacle" = [defaultObstacle (500, randYPos)]
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
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = fireBullet gstate
inputKey (EventKey (SpecialKey KeyEsc) Up _ _) gstate@GameState{paused} = gstate{paused = not paused}
inputKey (EventKey (Char c) d _ _) gstate
  | c == 'w' || c == 'a' || c == 's' || c == 'd' = updateKeys d
  | otherwise = gstate
  where
    updateKeys :: KeyState -> GameState
    updateKeys Down = gstate { downKeys = c : downKeys gstate }
    updateKeys Up   = gstate { downKeys = delete c (downKeys gstate) }
inputKey (EventKey (MouseButton LeftButton) Up _ _) gstate = fireBullet gstate
inputKey _ gstate = gstate

fireBullet :: GameState -> GameState
fireBullet gstate@GameState{player, playerBullets} = gstate {playerBullets = defaultPlayerBullet origin : playerBullets} where
  origin = playerPos player -- take the player's position as the origin of the bullet

-- | General helper functions



shootBullet :: (Shootable a, Destructible a, Destructible b) => a -> GameState -> [b] -> (Bool, GameState)
shootBullet b gstate@GameState{explosions} ds = case shoot b ds of
  (Miss, _) -> filter' b gstate
  (Damage i, Just d) -> (False, (destroy b . update i d) gstate)
  (Kill, Just d) -> (False, (destroy b . destroy d) gstate{explosions = defaultExplosion (getPosition d) : explosions})
  (_, _) -> (True, gstate)


  -- shootBullet pb gstate = case shoot pb ds of
  --   (Miss, _)      -> filter' pb gstate
  --   (Damage i, Just o@Obstacle{}) -> (destroy pb . update i o) gstate
  --   --(Damage i, Just x) -> undefined -- if x is an enemy: i is the index in the list ds, so convert i to index in list of enemies
  --   (Kill, Just o@Obstacle{obstaclePos}) -> (destroy pb . destroy o) gstate{explosions = defaultExplosion obstaclePos : explosions}
  --   (_, _)         -> gstate

-- Replace the element at the given index of the list with the given value
replace :: Int -> a -> [a] -> [a]
replace index x xs = zs ++ (x:ys)
  where (zs, _:ys) = splitAt index xs

-- Remove an object from gamestate if it's outside of bounds
filter' :: Destructible a => a -> GameState -> (Bool, GameState)
filter' a gstate
  | x > 550 || x < -550 = (False, destroy a gstate)
  | otherwise = (True, gstate)
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