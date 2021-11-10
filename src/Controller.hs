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

step :: Float -> GameState -> IO GameState
step secs gstate@GameState{score, paused, gameOver, timeElapsed, player, meteors, downKeys, saveLoad = (wantsToSave, wantsToLoad), explosions}
  -- Check if the player pressed the save or load key
  | wantsToSave = saveGame gstate
  | wantsToLoad = loadGame gstate
  | otherwise   = return (newState gstate)
  where
    newState :: GameState -> GameState
    newState oldState@GameState{gameOver, paused}
      | gameOver  = order66 (updateState secs oldState)
      | paused    = oldState -- No change to gamestate if game is paused
      | otherwise = updateState secs oldState

-- | Update the entire GameState using the various update functions
updateState :: Float -> GameState -> GameState
updateState secs gstate@GameState{gameOver, timeElapsed, score, player, activePUs, downKeys, meteors, explosions, powerUps, bgList} =
  (spawnObjects . updatePlayer . updateTurrets . updateDrones . updateKamikazes . updateEnemyBullets . updatePlayerBullets)
  gstate{score = updateScore gameOver secs score,
         deltaTime = secs,
         timeElapsed = timeElapsed + secs,
         player = p,
         activePUs = aps,
         explosions = updateExplosions secs explosions,
         meteors = map (move secs) meteors,
         bgList = updateBGList gstate secs,
         powerUps = filter withinBounds (map (move secs) powerUps)
        }
  where (aps, p) = updateActivePUs activePUs player secs

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

-- | Update player
updatePlayer :: GameState -> GameState
updatePlayer gstate@GameState{player, deltaTime, downKeys, turrets, drones, meteors, kamikazes, powerUps}
  = (checkPowerUps . checkPlayerCollision turrets . checkPlayerCollision drones . checkPlayerCollision meteors . checkPlayerCollision kamikazes)
    gstate{player = (movePlayer downKeys deltaTime player){
      playerFr = let (FireRate fr last) = playerFr player in FireRate fr (last + deltaTime),
      playerAnim = animateR deltaTime (playerAnim player)
    }}

movePlayer :: [Char] -> Float -> Player -> Player
movePlayer downKeys secs p@Player{playerSpeed} = changePosition p getMovement where
  getMovement :: (Float,Float)
  getMovement = foldr (checkKey (playerSpeed * secs)) (0,0) downKeys

-- Adds to the x y movement based on the key that is being held down
checkKey :: Float -> Char -> (Float, Float) -> (Float, Float)
checkKey n 's' (x,y) = (x, y - n)
checkKey n 'a' (x,y) = (x - n, y)
checkKey n 'w' (x,y) = (x, y + n)
checkKey n 'd' (x,y) = (x + n, y)
checkKey _ _   acc   = acc

-- Check collision of player with given enemies/obstacles
checkPlayerCollision :: Destructible a => [a] -> GameState -> GameState
checkPlayerCollision ds gstate = foldr check gstate ds where
  check :: Destructible a => a -> GameState -> GameState
  check d gstate'@GameState{player, explosions}
    | collide player d = case applyDamage player 45 of -- Colliding with any object or enemy does 45 damage (except power ups)
        (False, p) -> destroy d gstate{gameOver = True, player = p, explosions = defaultExplosion (getPosition d) : explosions}
        (True, p)  -> destroy d gstate{player = p, explosions = defaultExplosion (getPosition d) : explosions}
    | otherwise = gstate'

-- Check collision of playe with power ups and apply their effects if any are found
checkPowerUps :: GameState -> GameState
checkPowerUps gstate@GameState{player, powerUps} = foldr checkPowerUp gstate powerUps where
  checkPowerUp :: PowerUp -> GameState -> GameState
  checkPowerUp pu@PowerUp{puType} gstate@GameState{player = p@Player{playerHp, playerSpeed, playerFr = FireRate fr last}, activePUs}
    | collide player pu = case puType of
      Health n -> destroy pu gstate{player = p{playerHp = (max 100 (fst playerHp + n), snd playerHp)}}
      s@(Speed n _) -> destroy pu gstate{player = p{playerSpeed = playerSpeed + n}, activePUs = s : activePUs}
      f@(FR n _) -> destroy pu gstate{player = p{playerFr = FireRate (fr - n) last}, activePUs = f : activePUs}
      i@(Invincibility _) -> destroy pu gstate{player = p{playerHp = (fst playerHp, True)}, activePUs = i : activePUs}
    | otherwise = gstate

-- | Update player bullets
updatePlayerBullets :: GameState -> GameState
updatePlayerBullets gstate@GameState{deltaTime, playerBullets} = foldr shootPlayerBullet gstate{playerBullets = pbs} pbs where
  pbs = (filter withinBounds . map (move deltaTime)) playerBullets
  shootPlayerBullet :: PlayerBullet -> GameState -> GameState -- Check the collision of this bullet with meteors and all enemies
  shootPlayerBullet pb gstate@GameState{meteors, turrets, drones, kamikazes}
    = snd ((shootBullet meteors . shootBullet turrets . shootBullet drones . shootBullet kamikazes) (Just pb, gstate))

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
  = gstate{turrets = (filter withinBounds . map updateTurret) turrets, enemyBullets = foldr turretFire [] turrets ++ enemyBullets, generator = newGen} where
    updateTurret :: Turret -> Turret
    updateTurret t@Turret{turretFr, turretAnim} = move deltaTime t{turretFr = updateFr deltaTime turretFr, turretAnim = animateR deltaTime turretAnim}
    turretFire :: Turret -> [EnemyBullet] -> [EnemyBullet]
    turretFire Turret{turretPos, turretFr = FireRate fr last} acc
      | last + deltaTime > fr = defaultEnemyBullet turretPos (turretBulletOrient playerPos turretPos) : acc
      | otherwise = acc
    turretBulletOrient :: Point -> Point -> Float
    turretBulletOrient (px,py) pos = clampOrientation (anglePoints pos (px + randVal, py + randVal))
    (randVal, newGen) = randomR (-20, 20) generator

-- | Update drones
updateDrones :: GameState -> GameState
updateDrones gstate@GameState{deltaTime, drones, enemyBullets}
  = gstate{drones = (filter withinBounds . map updateDrone) drones, enemyBullets = foldr droneFire [] drones ++ enemyBullets} where
    updateDrone :: Drone -> Drone
    updateDrone d@Drone{droneFr, droneAnim} = move deltaTime d{droneFr = updateFr deltaTime droneFr, droneAnim = animateR deltaTime droneAnim}
    droneFire :: Drone -> [EnemyBullet] -> [EnemyBullet]
    droneFire Drone{dronePos, droneFr = FireRate fr last} acc
      | last + deltaTime > fr = droneBullets dronePos ++ acc
      | otherwise = acc
    droneBullets :: Point -> [EnemyBullet]
    droneBullets pos = map (defaultEnemyBullet pos) [0.75 * pi, 0.25 * pi, -0.25 * pi, -0.75 * pi, pi, 0, 0.5 * pi, -0.5 * pi]

-- | Update Kamikazes
updateKamikazes :: GameState -> GameState
updateKamikazes gstate@GameState{player, deltaTime, kamikazes}
  = gstate{kamikazes = (filter withinBounds . map updateKamikaze) kamikazes} where
    updateKamikaze :: Kamikaze -> Kamikaze
    updateKamikaze k = move deltaTime k{kamikazeOrient = kamikazeOrientation k}
    kamikazeOrientation :: Kamikaze -> Float
    kamikazeOrientation k@Kamikaze{kamikazePos, kamikazeOrient} -- Only change orientation if player is within 'sight'
      | (angle > -0.75 * pi && angle < 0) || (angle < 0.75 * pi && angle > 0) = kamikazeOrient
      | otherwise = angle
      where angle = anglePoints kamikazePos (playerPos player)

-- | Update backgrounds
updateBGList :: GameState -> Float -> [Background]
updateBGList gstate@GameState{paused, gameOver, bgList} secs
  | paused = bgList
  | gameOver = bgList
  | otherwise = map f bgList where
        f bg@Background {backgroundXPos, backgroundSpeed} =
          if backgroundXPos > -1024 then bg {backgroundXPos = backgroundXPos + (backgroundSpeed * secs)} else bg {backgroundXPos = 1024}

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
    (randSpeed, newGen'') = randomR (-250, -150) newGen :: (Float, StdGen)

-- Returns enemy info if at least one enemy has yet to be spawned
spawnInfo :: SpawnList -> (Float, String, SpawnList)
spawnInfo eList@(SpawnList (SpawnListItem enemyTimer enemyType : xs)) = (enemyTimer, enemyType, SpawnList xs)
spawnInfo eList@(SpawnList []) = (1.0 / 0, "", eList)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyEsc) Up _ _) gstate@GameState{paused} = gstate{paused = not paused}
inputKey (EventKey (Char c) Up _ _) gstate@GameState{paused, downKeys}
  | c == 'w' || c == 'a' || c == 's' || c == 'd' = gstate { downKeys = delete c downKeys}
  | paused && c == 'o' = gstate{saveLoad = (True, False)}
  | paused && c == 'p' = gstate{saveLoad = (False, True)}
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

-- | General helper functions that depend on certain type classes (can't define those in Helper.hs)

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

-- Checks whether an object is within bounds
withinBounds :: Positionable a => a -> Bool
withinBounds a
  | x > 550 || x < -550 || y > 400 || y < -400 = False
  | otherwise = True
  where (x,y) = getPosition a