{-# LANGUAGE NamedFieldPuns #-}

-- Contains all custom type classes and their instances
module Classes where

import Model
import Helper ( replace, clamp, draw, animateR )

import Graphics.Gloss ( Point, red, color, line, rotate, translate, Picture, scale, Vector )
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Geometry.Line (segClearsBox)
import Data.List ( delete, find, elemIndex )

-- | Positionable type class
class Positionable a where
  getPosition :: a -> Point
  getOrientation :: a -> Float
  changePosition :: a -> Vector -> a

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
  update :: [a] -> Destructibles -> Destructibles -- Updates list of this Destructible in the Destructbiles object

instance Destructible Player where
  applyDamage player@Player {playerHp = (hp, invincible)} damage
    | invincible = (True, player) -- If invincibility is active, no damage is taken
    | hp - damage <= 0 = (False, player{playerHp = (0, invincible)})
    | otherwise = (True, player{playerHp = (hp - damage, invincible)})
  update [p] (EnemyDS ms _) = EnemyDS ms p
  update [] (EnemyDS ms p) = EnemyDS ms p{playerHp = (0, False)}
  update _ x = x

instance Destructible Turret where
  applyDamage t@Turret{turretHp} damage
    | turretHp - damage <= 0 = (False, t)
    | otherwise = (True, t {turretHp = turretHp - damage})
  update ts (PlayerDS ms _ ds ks) = PlayerDS ms ts ds ks
  update _ x = x

instance Destructible Drone where
  applyDamage d@Drone{droneHp} damage
    | droneHp - damage <= 0 = (False, d)
    | otherwise = (True, d {droneHp = droneHp - damage})
  update ds (PlayerDS ms ts _ ks) = PlayerDS ms ts ds ks
  update _ x = x

instance Destructible Kamikaze where
  applyDamage k@Kamikaze{kamikazeHp} damage
    | kamikazeHp - damage <= 0 = (False, k)
    | otherwise = (True, k {kamikazeHp = kamikazeHp - damage})
  update ks (PlayerDS ms ts ds _) = PlayerDS ms ts ds ks
  update _ x = x

instance Destructible Meteor where
  applyDamage obs@Meteor{meteorHp} damage
    | meteorHp - damage <= 0 = (False, obs)
    | otherwise = (True, obs {meteorHp = meteorHp - damage})
  update ms (PlayerDS _ ts ds ks) = PlayerDS ms ts ds ks
  update ms (EnemyDS _ p) = EnemyDS ms p

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

instance Moveable Explosion where
  getSpeed = explosionSpeed

-- | Shootable type class
class (Positionable a, Collideable a) => Shootable a where
  getDmg :: a -> Int
  shoot :: Destructible b => a -> [b] -> (HitInfo, Maybe b)
  shoot b xs = case find (collide b) xs of
    Just x -> case applyDamage x (getDmg b) of
      -- If the hit doesn't kill the object being hit, also return the index to update the object in its list
      (True, y)  -> (Damage i, Just y)
      (False, y) -> (Kill i, Just y)
      where (Just i) = elemIndex x xs
    Nothing -> (Miss, Nothing)

data HitInfo = Miss | Damage Int | Kill Int

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

instance Shootable EnemyBullet where
  getDmg EnemyBullet{ebDmg} = ebDmg