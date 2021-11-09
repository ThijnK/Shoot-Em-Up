{-# LANGUAGE NamedFieldPuns #-}

-- Contains all custom type classes and their instances
module Classes where

import Model
import Helper ( replace, clamp )

import Graphics.Gloss ( Point, red, color, line, rotate, translate, Picture, scale )
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Geometry.Line (segClearsBox)
import Data.List ( delete, find, elemIndex )

-- | Positionable type class
class Positionable a where
  getPosition :: a -> Point
  getOrientation :: a -> Float
  changePosition :: a -> Point -> a
  -- changeOrientation :: a -> Float -> a

instance Positionable Player where
  getPosition = playerPos
  getOrientation = playerOrient
  changePosition p@Player{playerPos = (x,y)} (mx,my) = p{playerPos = (clamp (x + mx) (-500,0), clamp (y + my) (-300,300))}

instance Positionable Turret where
  getPosition = turretPos
  getOrientation = turretOrient
  changePosition t@Turret{turretPos = (x,y)} (mx,my) = t{turretPos = (x + mx, y + my)}

instance Positionable Drone where
  getPosition = dronePos
  getOrientation = droneOrient
  changePosition d@Drone{dronePos = (x,y)} (mx,my) = d{dronePos = (x + mx, y + my)}

instance Positionable Kamikaze where
  getPosition = kamikazePos
  getOrientation = kamikazeOrient
  changePosition k@Kamikaze{kamikazePos = (x,y)} (mx,my) = k{kamikazePos = (x + mx, y + my)}

instance Positionable PlayerBullet where
  getPosition = pbPos
  getOrientation = pbOrient
  changePosition pb@PlayerBullet{pbPos = (x,y)} (mx,my) = pb{pbPos = (x + mx, y + my)}

instance Positionable EnemyBullet where
  getPosition = ebPos
  getOrientation = ebOrient
  changePosition eb@EnemyBullet{ebPos = (x,y)} (mx,my) = eb{ebPos = (x + mx, y + my)}

instance Positionable Meteor where
  getPosition = meteorPos
  getOrientation = meteorOrient
  changePosition o@Meteor{meteorPos = (x,y)} (mx,my) = o{meteorPos = (x + mx, y + my)}

instance Positionable Explosion where
  getPosition = explosionPos
  getOrientation = explosionOrient
  changePosition e@Explosion{explosionPos = (x,y)} (mx,my) = e{explosionPos = (x + mx, y + my)}

instance Positionable PowerUp where
  getPosition = puPos
  getOrientation = puOrient
  changePosition pu@PowerUp{puPos = (x,y)} (mx,my) = pu{puPos = (x + mx, y + my)}

-- | Drawable type class
class Positionable a => Drawable a where
  getSprite :: Sprites -> a -> Picture
  toPicture :: Sprites -> a -> Picture
  toPicture sprites x = draw (getOrientation x) (getPosition x) (getSprite sprites x)

instance Drawable Player where
  getSprite Sprites{playerSprites} Player{playerAnim = Animation index _ _ _} = playerSprites !! index

instance Drawable Turret where
  getSprite Sprites{turretSprites} Turret{turretAnim = Animation index _ _ _} = turretSprites !! index
  toPicture sprites t = draw 0 (getPosition t) (getSprite sprites t)

instance Drawable Drone where
  getSprite Sprites{droneSprites} Drone{droneAnim = Animation index _ _ _} = droneSprites !! index

instance Drawable Kamikaze where
  getSprite Sprites{kamikazeSprite} _ = kamikazeSprite
  -- toPicture Sprites{kamikazeSprite} Kamikaze{kamikazePos, kamikazeOrient} = scale 0.8 0.8 (draw kamikazeOrient kamikazePos kamikazeSprite)

instance Drawable PlayerBullet where
  getSprite Sprites{pBulletSprite} _ = pBulletSprite

instance Drawable EnemyBullet where
  getSprite Sprites{eBulletSprite} _ = eBulletSprite

instance Drawable Meteor where
  getSprite Sprites{meteorSprite} _ = meteorSprite

instance Drawable Explosion where
  getSprite Sprites{explosionSprites} Explosion{explosionAnim = Animation index _ _ _} = explosionSprites !! index

instance Drawable PowerUp where
  getSprite Sprites{hpPowerUp} PowerUp{puType = Health _} = hpPowerUp
  getSprite Sprites{speedPowerUp} PowerUp{puType = Speed _ _} = speedPowerUp
  getSprite Sprites{frPowerUp} PowerUp{puType = FR _ _} = frPowerUp
  getSprite Sprites{invincPowerUp} PowerUp{puType = Invincibility _} = invincPowerUp

draw :: Float -> Point -> Picture -> Picture
draw orientation (x,y) = translate x y . rotate (radToDeg (-orientation))

drawHbox :: (Positionable a, Collideable a) => a -> Picture
drawHbox a = color red (line [(x-w,y-h),(x+w,y-h),(x+w,y+h),(x-w,y+h),(x-w,y-h)]) where
    (x,y) = getPosition a
    (w,h) = getHitbox a

-- | Collideable type class
class Collideable a where
  getHitbox :: a -> Point

instance Collideable Player where
  getHitbox = playerHbox

instance Collideable Turret where
  getHitbox = turretHbox

instance Collideable Drone where
  getHitbox = droneHbox

instance Collideable Kamikaze where
  getHitbox = kamikazeHbox

instance Collideable PlayerBullet where
  getHitbox = pbHbox

instance Collideable EnemyBullet where
  getHitbox = ebHbox

instance Collideable Meteor where
  getHitbox = meteorHbox

instance Collideable PowerUp where
  getHitbox = puHbox

-- | Destructible type class
class (Positionable a, Collideable a, Eq a) => Destructible a where
  applyDamage :: a -> Int -> (Bool, a) -- For the Bool value: True means alive, False means dead
  destroy :: a -> GameState -> GameState
  update :: Int -> a -> GameState -> GameState

instance Destructible Player where
  applyDamage player@Player {playerHp = (hp, Invincibility n)} damage
    | n > 0 = (True, player) -- If invincibility is active, no damage is taken
    | hp - damage <= 0 = (False, player{playerHp = (hp - damage, Invincibility n)})
    | otherwise = (True, player{playerHp = (hp - damage, Invincibility n)})
  applyDamage player _ = (True, player)
  destroy p gstate = update 0 p{playerHp = (0, Invincibility 0)} gstate{gameOver = True}
  update _ p gstate = gstate{player = p}

instance Destructible Turret where
  applyDamage t@Turret{turretHp} damage
    | turretHp - damage <= 0 = (False, t)
    | otherwise = (True, t {turretHp = turretHp - damage})
  destroy t gstate@GameState{turrets} = gstate{turrets = delete t turrets}
  update i t gstate@GameState{turrets} = gstate{turrets = replace i t turrets}

instance Destructible Drone where
  applyDamage d@Drone{droneHp} damage
    | droneHp - damage <= 0 = (False, d)
    | otherwise = (True, d {droneHp = droneHp - damage})
  destroy d gstate@GameState{drones} = gstate{drones = delete d drones}
  update i d gstate@GameState{drones} = gstate{drones = replace i d drones}

instance Destructible Kamikaze where
  applyDamage k@Kamikaze{kamikazeHp} damage
    | kamikazeHp - damage <= 0 = (False, k)
    | otherwise = (True, k {kamikazeHp = kamikazeHp - damage})
  destroy k gstate@GameState{kamikazes} = gstate{kamikazes = delete k kamikazes}
  update i k gstate@GameState{kamikazes} = gstate{kamikazes = replace i k kamikazes}

instance Destructible Meteor where
  applyDamage obs@Meteor{meteorHp} damage
    | meteorHp - damage <= 0 = (False, obs)
    | otherwise = (True, obs {meteorHp = meteorHp - damage})
  destroy o gstate@GameState{meteors} = gstate{meteors = delete o meteors}
  update i o gstate@GameState{meteors} = gstate{meteors = replace i o meteors}

instance Destructible PlayerBullet where
  applyDamage = undefined -- not used
  destroy pb gstate@GameState{playerBullets} = gstate{playerBullets = delete pb playerBullets}
  update i pb gstate@GameState{playerBullets} = gstate{playerBullets = replace i pb playerBullets}

instance Destructible EnemyBullet where
  applyDamage = undefined
  destroy eb gstate@GameState{enemyBullets} = gstate{enemyBullets = delete eb enemyBullets}
  update i eb gstate@GameState{enemyBullets} = gstate{enemyBullets = replace i eb enemyBullets}

instance Destructible PowerUp where
  applyDamage = undefined
  destroy pu gstate@GameState{powerUps} = gstate{powerUps = delete pu powerUps}
  update i pu gstate@GameState{powerUps} = gstate{powerUps = replace i pu powerUps}

-- | Moveable type class
class Positionable a => Moveable a where
  getSpeed :: a -> Float
  move :: Float -> a -> a
  move secs a = changePosition a (secs * speed * cos orient, secs * speed * sin orient) where
    orient = getOrientation a
    speed = getSpeed a

instance Moveable PlayerBullet where
  getSpeed = pbSpeed

instance Moveable Meteor where
  getSpeed = meteorSpeed

instance Moveable Turret where
  getSpeed = turretSpeed
  -- Custom movement for turrets
  move secs t@Turret{turretPos = (x, y), turretOrient, turretSpeed, turretTarget}
    | x < turretTarget = changePosition t{turretOrient = turretOrient + 0.05, turretTarget = turretTarget + 50} (secs * turretSpeed * cos turretOrient, secs * turretSpeed * sin turretOrient)
    | otherwise        = changePosition t (secs * turretSpeed, 0)

instance Moveable Drone where
  getSpeed = droneSpeed

instance Moveable Kamikaze where
  getSpeed = kamikazeSpeed

instance Moveable EnemyBullet where
  getSpeed = ebSpeed

instance Moveable PowerUp where
  getSpeed = puSpeed

-- | Shootable type class
class (Positionable a, Collideable a) => Shootable a where
  getDmg :: a -> Int
  shotByPlayer :: a -> Bool -- Whether or not this shootable thing was shot by the player (mainly for future proofing)
  shoot :: Destructible b => a -> [b] -> (HitInfo, Maybe b)
  shoot b xs = case find (collide b) xs of
    Just x -> case applyDamage x (getDmg b) of
      (True, y)  -> (Damage i, Just y)
      (False, y) -> (Kill, Just y)
      where (Just i) = elemIndex x xs
    Nothing -> (Miss, Nothing)

data HitInfo = Miss | Damage Int | Kill

-- Check collision of two collideables
collide :: (Positionable a, Positionable b, Collideable a, Collideable b) => a -> b -> Bool
collide a b = not (segClearsBox (xa - wa, ya - ha) (xa + wa, ya + ha) ll ur)
              || not (segClearsBox (xa - wa, ya + ha) (xa + wa, ya - ha) ll ur)
  where
    (xa,ya) = getPosition a
    (wa,ha) = getHitbox a
    ll = (xb - wb, yb - hb)
    ur = (xb + wb, yb + hb)
    (xb,yb) = getPosition b
    (wb,hb) = getHitbox b

instance Shootable PlayerBullet where
  getDmg PlayerBullet{pbDmg} = pbDmg
  shotByPlayer _ = True 

instance Shootable EnemyBullet where
  getDmg EnemyBullet{ebDmg} = ebDmg
  shotByPlayer _ = False 