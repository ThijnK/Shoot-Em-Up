{-# LANGUAGE NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Classes
import Defaults
import SaveLoad ( saveGame, loadGame )
import Helper

import Graphics.Gloss ( Point )
import Graphics.Gloss.Interface.IO.Game
import System.Random ( Random(randomR), StdGen )
import Data.List ( delete )
import Data.Maybe ( mapMaybe )
import Graphics.Gloss.Data.Vector

step :: Float -> GameState -> IO GameState
step secs gstate@GameState{score, paused, gameOver, timeElapsed, player, meteors, downKeys, saveLoad = (wantsToSave, wantsToLoad), explosions}
  -- Check if the player pressed the save or load key
  | wantsToSave = saveGame gstate
  | wantsToLoad = loadGame gstate
  | otherwise   = return (newState gstate)
  where
    newState :: GameState -> GameState
    newState oldState@GameState{gameOver, paused, explosions}
      | gameOver  = order66 oldState{explosions = updateExplosions secs explosions}
      | paused    = oldState -- No change to gamestate if game is paused
      | otherwise = updateState secs oldState

-- | Update the entire GameState using the various update functions
updateState :: Float -> GameState -> GameState
updateState secs gstate@GameState{paused, deltaTime, gameOver, timeElapsed, score, playerBullets, enemyBullets, player, activePUs, downKeys, meteors, turrets, drones, kamikazes, explosions, powerUps, bgList, generator}
  = spawnObjects gstate{
    gameOver = go,
    score = updateScore gameOver secs scoreIncr score,
    deltaTime = secs,
    timeElapsed = timeElapsed + secs,
    playerBullets = (filter withinBounds . map (move secs)) pbs,
    enemyBullets = newEbs ++ (filter withinBounds . map (move secs)) ebs,
    player = updatePlayer downKeys secs updatedPlayer,
    activePUs = aps ++ aps',
    turrets = (filter withinBounds . map (updateTurret secs)) updatedTurrets,
    drones = (filter withinBounds . map (updateDrone secs)) updatedDrones,
    kamikazes = (filter withinBounds . map (updateKamikaze secs (playerPos updatedPlayer))) updatedKamikazes,
    explosions = map (move secs) (updateExplosions secs (explosions ++ es1 ++ es2 ++ es3)),
    meteors = (filter withinBounds . map (move secs) )updatedMeteors,
    bgList = updateBGList bgList paused go secs,
    powerUps = filter withinBounds (map (move secs) ps)
  }
  where
    go                                               = checkGameOver ((fst . playerHp) updatedPlayer)
    newEbs                                           = fst (foldr (turretFire secs (playerPos updatedPlayer)) ([], generator) updatedTurrets) ++ foldr (droneFire secs) [] updatedDrones
    (ebs, EnemyDS updatedMeteors updatedPlayer, es3) = updateEnemyBullets enemyBullets eds
    eds                                              = EnemyDS ms p3
    (pbs, PlayerDS ms updatedTurrets updatedDrones updatedKamikazes, es2, scoreIncr) = updatePlayerBullets playerBullets pds'
    (p3, pds', es1) = checkPlayerCollisions p2 pds
    (p2, ps, aps')      = checkPowerUps p1 powerUps
    (aps, p1)           = updateActivePUs activePUs player secs
    pds                 = PlayerDS meteors turrets drones kamikazes

-- | Clear all objects and spawn explosions
order66 :: GameState -> GameState
order66 gstate@GameState{player, meteors, turrets, drones, kamikazes, explosions}
  = gstate{player = player{playerHp = (0,False)}, meteors = [], turrets = [], drones = [], kamikazes = [], playerBullets = [], enemyBullets = [], powerUps = [], explosions = es ++ explosions}
  where es = map defaultExplosion (map getPosition meteors ++ map getPosition turrets ++ map getPosition drones ++ map getPosition kamikazes)

-- | Check if the it's a game over (player hp is <= 0)
checkGameOver :: Int -> Bool
checkGameOver playerHp
  | playerHp <= 0 = True
  | otherwise     = False

-- | Update score
updateScore :: Bool -> Float -> Int -> Score -> Score
updateScore True _ _ s = s
updateScore False secs scoreIncrease s@(Score n incr last)
  | last + secs > incr = Score (n + 1 + scoreIncrease) incr 0
  | otherwise = Score (n + scoreIncrease) incr (last + secs)

-- Score that the player receives for killing an enemy
scoreForKill :: Int 
scoreForKill = 10

-- | Update active power ups
updateActivePUs :: [PowerUpType] -> Player -> Float -> ([PowerUpType], Player)
updateActivePUs ps p secs = foldr updatePU ([],p) ps where
  updatePU :: PowerUpType -> ([PowerUpType], Player) -> ([PowerUpType], Player)
  updatePU pu@(Speed n t) (ps, p) = passPU (Speed n (t - secs)) t ps p (\p -> p{playerSpeed = playerSpeed p - n})
  updatePU pu@(FR n t) (ps, p)    = passPU (FR n (t - secs)) t ps p (\p@Player{playerFr = FireRate fr last} -> p{playerFr = FireRate (fr + n) last})
  updatePU pu@(Invincibility t) (ps, p) = passPU (Invincibility (t - secs)) t ps p (\p -> p{playerHp = ((fst . playerHp) p, False)})
  updatePU _ acc = acc
  -- Only adds the given PU to the list if it's still active
  passPU :: PowerUpType -> Float -> [PowerUpType] -> Player -> (Player -> Player) -> ([PowerUpType], Player)
  passPU pu t ps p f
    | t <= 0 = (ps, f p)
    | otherwise = (pu : ps, p)

-- | Move player
updatePlayer :: [Char] -> Float -> Player -> Player
updatePlayer downKeys secs p@Player{playerSpeed, playerAnim, playerFr = FireRate fr last} 
  = changePosition p{playerAnim = animateR secs playerAnim, playerFr = FireRate fr (last + secs)} getMovement where
  getMovement :: (Float,Float)
  -- If the player presses two keys at the same time (move diagonally) we need to make sure it doesn't move faster than intended
  getMovement
    | x > 0 && y > 0 = mulSV (playerSpeed * secs) (normalizeV v) -- Normalize movement and then multiply by the actual distance that it should move
    | otherwise = v
    where v@(x,y) = foldr (checkKey (playerSpeed * secs)) (0,0) downKeys

-- Adds to the x y movement based on the key that is being held down
checkKey :: Float -> Char -> (Float, Float) -> (Float, Float)
checkKey n 's' (x,y) = (x, y - n)
checkKey n 'a' (x,y) = (x - n, y)
checkKey n 'w' (x,y) = (x, y + n)
checkKey n 'd' (x,y) = (x + n, y)
checkKey _ _   acc   = acc

-- | Deal with collisions of the player

-- Deal this amount of damage to the player when they collide with something
playerCollisionDmg :: Int 
playerCollisionDmg = 40

-- Check collisions with enemies
checkPlayerCollisions :: Player -> Destructibles -> (Player, Destructibles, [Explosion])
checkPlayerCollisions player ds@(PlayerDS ms ts drs ks) = (checkObjects ms . checkObjects ts . checkObjects drs . checkObjects ks) (player, ds, []) where
  checkObjects :: Destructible a => [a] -> (Player, Destructibles, [Explosion]) -> (Player, Destructibles, [Explosion])
  checkObjects xs (p, ds, es) = let (p', xs', es') = foldr checkObject (p,[],es) xs in (p', update xs' ds, es')
  checkObject :: Destructible a => a -> (Player, [a], [Explosion]) -> (Player, [a], [Explosion])
  checkObject d (player, ds, es)
    | collide player d = (snd (applyDamage player playerCollisionDmg), ds, defaultExplosion (getPosition d) : es)
    | otherwise = (player, d : ds, es)
checkPlayerCollisions p ds = (p, ds, [])

-- Check player collisions with power ups
checkPowerUps :: Player -> [PowerUp] -> (Player, [PowerUp], [PowerUpType])
checkPowerUps player = foldr checkPowerUp (player, [], []) where
  checkPowerUp :: PowerUp -> (Player, [PowerUp], [PowerUpType]) -> (Player, [PowerUp], [PowerUpType])
  checkPowerUp pu@PowerUp{puType} (p@Player{playerHp, playerSpeed, playerFr = FireRate fr last}, ps, activePUs)
    | collide player pu = case puType of
      Health n -> (p{playerHp = (fst playerHp + n, snd playerHp)}, ps, activePUs)
      s@(Speed n _) -> (p{playerSpeed = playerSpeed + n}, ps, s : activePUs)
      f@(FR n _) -> (p{playerFr = FireRate (fr - n) last}, ps, f : activePUs)
      i@(Invincibility _) -> (p{playerHp = (fst playerHp, True)}, ps, i : activePUs)
    | otherwise = (p, pu : ps, activePUs)

-- | Update player bullets
updatePlayerBullets :: [PlayerBullet] -> Destructibles -> ([PlayerBullet], Destructibles, [Explosion], Int)
updatePlayerBullets pbs ds = foldr shootPlayerBullet ([], ds, [], 0) pbs where
  shootPlayerBullet :: PlayerBullet -> ([PlayerBullet], Destructibles, [Explosion], Int) -> ([PlayerBullet], Destructibles, [Explosion], Int) -- Check the collision of this bullet with meteors and all enemies
  shootPlayerBullet pb (pbs, ds@(PlayerDS ms ts drs ks), es, score)
    = case (shootBullet ms . shootBullet ts . shootBullet drs . shootBullet ks) (Just pb, ds, es) of
      (Nothing, ds', []) -> (pbs, ds', [], score)
      (Nothing, ds', es') -> (pbs, ds', es', score + scoreForKill)
      (Just pb', ds', es') -> (pb' : pbs, ds', es', score)
  shootPlayerBullet _ x = x

-- | Update enemy bullets
updateEnemyBullets :: [EnemyBullet] -> Destructibles -> ([EnemyBullet], Destructibles, [Explosion])
updateEnemyBullets ebs ds = foldr shootEnemyBullet ([], ds, []) ebs where
  shootEnemyBullet :: EnemyBullet -> ([EnemyBullet], Destructibles, [Explosion]) -> ([EnemyBullet], Destructibles, [Explosion]) -- Check collision of this bullet with meteors and the player
  shootEnemyBullet eb (ebs, ds@(EnemyDS ms p), es) = case (shootBullet ms . shootBullet [p]) (Just eb, ds, es) of
    (Nothing, ds', es') -> (ebs, ds', es')
    (Just eb, ds', es') -> (eb : ebs, ds', es')
  shootEnemyBullet _ x = x

-- Checks collision for a given bullet and a list of obstacles it can hit
shootBullet :: (Shootable a, Destructible b) => [b] -> (Maybe a, Destructibles, [Explosion]) -> (Maybe a, Destructibles, [Explosion])
shootBullet xs (Just b, ds, es) = case shoot b xs of
  (Damage i, Just d) -> (Nothing, update (replace i d xs) ds, es)
  (Kill i, Just d)     -> (Nothing, update (deleteAt i xs) ds, defaultExplosion (getPosition d) : es)
  (_, _)             -> (Just b, ds, es)
shootBullet _ x@(Nothing, _, _) = x -- Return everything unchanged if the bullet already hit something

-- | Update explosions
updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions secs = mapMaybe (\e@Explosion{explosionAnim} -> animateM secs explosionAnim >>= (\x -> return e{explosionAnim = x}))

-- | Update turrets
updateTurret :: Float -> Turret -> Turret
updateTurret secs t@Turret{turretFr, turretAnim} = move secs t{turretFr = updateFr secs turretFr, turretAnim = animateR secs turretAnim}

-- Get bullets fired by turrets
turretFire :: Float -> Point -> Turret -> ([EnemyBullet], StdGen) -> ([EnemyBullet], StdGen)
turretFire secs target Turret{turretPos, turretFr = FireRate fr last} (ebs, gen)
  | last + secs > fr = (defaultEnemyBullet turretPos (turretBulletOrient gen target turretPos) : ebs, newGen)
  | otherwise = (ebs, gen)
  where
    turretBulletOrient :: StdGen -> Point -> Point -> Float
    turretBulletOrient gen (px,py) pos = clampOrientation (anglePoints pos (px + randVal, py + randVal))
    (randVal, newGen) = randomR (-20, 20) gen

-- | Update drones
updateDrone :: Float -> Drone -> Drone
updateDrone secs d@Drone{droneFr, droneAnim} = move secs d{droneFr = updateFr secs droneFr, droneAnim = animateR secs droneAnim}

-- Get bullets fired by drones
droneFire :: Float -> Drone -> [EnemyBullet] -> [EnemyBullet]
droneFire secs Drone{dronePos, droneFr = FireRate fr last} acc
  | last + secs > fr = droneBullets dronePos ++ acc
  | otherwise = acc
  where
    droneBullets :: Point -> [EnemyBullet]
    droneBullets pos = map (defaultEnemyBullet pos) [0.75 * pi, 0.25 * pi, -0.25 * pi, -0.75 * pi, pi, 0, 0.5 * pi, -0.5 * pi]

-- | Update Kamikazes
updateKamikaze :: Float -> Point -> Kamikaze -> Kamikaze
updateKamikaze secs target k = move secs k{kamikazeOrient = kamikazeOrientation k} where
  kamikazeOrientation :: Kamikaze -> Float
  kamikazeOrientation k@Kamikaze{kamikazePos, kamikazeOrient} -- Only change orientation if player is within 'sight'
    | (angle > -0.75 * pi && angle < 0) || (angle < 0.75 * pi && angle > 0) = kamikazeOrient
    | otherwise = angle
    where angle = anglePoints kamikazePos target

-- | Update backgrounds
updateBGList :: [Background] -> Bool -> Bool -> Float -> [Background]
updateBGList bgList paused gameOver secs
  | paused = bgList
  | gameOver = bgList
  | otherwise = map updateBackground bgList
  where
    updateBackground bg@Background {backgroundXPos, backgroundSpeed}
      | backgroundXPos > -1024 = bg {backgroundXPos = backgroundXPos + (backgroundSpeed * secs)}
      | otherwise = bg {backgroundXPos = 1024}

-- | Spawn objects based on the spawnList
spawnObjects :: GameState -> GameState
spawnObjects gstate@GameState{timeElapsed, spawnList, meteors, turrets, drones, kamikazes, generator, powerUps}
  | timeElapsed > spawnTimer = spawnObject spawnType
  | otherwise = gstate
  where
    (spawnTimer, spawnType, newList) = spawnInfo (fst spawnList)
    spawnObject :: String -> GameState
    spawnObject "Turret" = gstate{spawnList = (newList, snd spawnList), turrets = defaultTurret (500, randYPos) randXPos : turrets, generator = newGen'}
    spawnObject "Drone"  = gstate{spawnList = (newList, snd spawnList), drones = defaultDrone (500, randYPos) : drones, generator = newGen}
    spawnObject "Meteor" = gstate{spawnList = (newList, snd spawnList), meteors = defaultMeteor (500, randYPos) randSpeed : meteors, generator = newGen''}
    spawnObject "Kamikaze" = gstate{spawnList = (newList, snd spawnList), kamikazes = defaultKamikaze (500, randYPos) : kamikazes, generator = newGen}
    spawnObject "HealthPU" = gstate{spawnList = (newList, snd spawnList), powerUps = defaultPowerUp (500, randYPos) defaultHpPU : powerUps, generator = newGen}
    spawnObject "SpeedPU" = gstate{spawnList = (newList, snd spawnList), powerUps = defaultPowerUp (500, randYPos) defaultFrPU : powerUps, generator = newGen}
    spawnObject "FireRatePU" = gstate{spawnList = (newList, snd spawnList), powerUps = defaultPowerUp (500, randYPos) defaultSpeedPU : powerUps, generator = newGen}
    spawnObject "InvincPU" = gstate{spawnList = (newList, snd spawnList), powerUps = defaultPowerUp (500, randYPos) defaultInvincPU : powerUps, generator = newGen}
    spawnObject _        = gstate{spawnList = (newList, snd spawnList), generator = newGen}
    (randYPos, newGen) = randomR (-250, 250) generator :: (Float, StdGen)
    (randXPos, newGen') = randomR (0, 450) newGen :: (Float, StdGen)
    (randSpeed, newGen'') = randomR (-200, -70) newGen :: (Float, StdGen)

-- Returns enemy info if at least one enemy has yet to be spawned
spawnInfo :: SpawnList -> (Float, String, SpawnList)
spawnInfo eList@(SpawnList (SpawnListItem enemyTimer enemyType : xs)) = (enemyTimer, enemyType, SpawnList xs)
spawnInfo eList@(SpawnList []) = (1.0 / 0, "", eList)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyEsc) Up _ _) gstate@GameState{paused} = gstate{paused = not paused}
inputKey (EventKey (Char c) Up _ _) gstate@GameState{paused, gameOver, downKeys}
  | c == 'w' || c == 'a' || c == 's' || c == 'd' = gstate { downKeys = delete c downKeys}
  -- Make sure you can only save and load when the game is paused (or game over in the case of loading)
  | paused && c == 'o' = gstate{saveLoad = (True, False)}
  | (paused && c == 'p') || (gameOver && c == 'p') = gstate{saveLoad = (False, True)}
  | otherwise = gstate
inputKey (EventKey (Char c) Down _ _) gstate@GameState{paused, downKeys}
  | c == 'w' || c == 'a' || c == 's' || c == 'd' = gstate { downKeys = c : downKeys }
  | otherwise = gstate
inputKey (EventKey (MouseButton LeftButton) Up _ _) gstate@GameState{paused, gameOver, deltaTime, player, playerBullets}
  | gameOver || paused = gstate
  | otherwise = let (p, pbs) = firePlayerBullet deltaTime gameOver player in gstate{player = p, playerBullets = pbs ++ playerBullets}
inputKey (EventKey (SpecialKey KeyEnter) Up _ _) gstate@GameState{gameOver, sprites, spawnList, generator}
  | gameOver = initialState sprites (snd spawnList) generator -- If the game is over and you press [Enter], you start over
  | otherwise = gstate
inputKey _ gstate = gstate

-- Fires a player bullet from the current position of the player
firePlayerBullet :: Float -> Bool -> Player -> (Player, [PlayerBullet])
firePlayerBullet secs False p@Player{playerPos, playerFr = FireRate fr last}
  | last + secs > fr = (p{playerFr = FireRate fr 0}, [defaultPlayerBullet playerPos])
  | otherwise = (p, [])
firePlayerBullet _ _ p = (p,[])

-- Checks whether an object is within bounds
withinBounds :: Positionable a => a -> Bool
withinBounds a
  | x > 550 || x < -550 || y > 400 || y < -400 = False
  | otherwise = True
  where (x,y) = getPosition a