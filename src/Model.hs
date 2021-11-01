{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Geometry.Line ( segClearsBox )
import Data.Maybe
import Data.List
import Data.Aeson
import GHC.Generics
import System.Random

data GameState = GameState {
  score         :: Int,
  paused        :: Bool,
  timeElapsed   :: Float,
  downKeys      :: [Char],
  player        :: Player,
  playerBullets :: [PlayerBullet],
  obstacles     :: [Obstacle],
  explosions    :: [Explosion],
  sprites       :: Sprites,
  enemyList     :: EnemyList,
  generator     :: StdGen 
}

initialState :: Sprites -> EnemyList -> StdGen -> GameState
initialState sprites enemyList generator = GameState {
  score         = 0,
  paused        = False,
  timeElapsed   = 0.0,
  downKeys      = [],
  player        = Player {
    playerPos = (-100, 0),
    playerOrient = 0,
    playerHp = 100,
    playerSpeed = 5,
    playerFr = 1,
    playerHbox = (13, 8),
    playerAnim = Anim 0 8
  },
  playerBullets = [],
  obstacles     = [defaultObstacle],
  explosions    = [],
  sprites       = sprites,
  enemyList     = enemyList,
  generator     = generator
}

defaultObstacle :: Obstacle
defaultObstacle = Obstacle (0, 0) 3.1415926535897932384626 5 50 (10, 10)

data Animation = Anim Int Int -- currentSpriteIndex and totalSpriteCount
  deriving Eq

data Sprites  = Sprites {
  playerSprites    :: [Picture],
  pBulletSprite    :: Picture,
  obstacleSprite   :: Picture,
  explosionSprites :: [Picture]
}

data Explosion = Explosion {
  explosionPos    :: Point,
  explosionOrient :: Float,
  explosionAnim   :: Animation
}

data Player = Player {
  playerPos    :: Point,
  playerOrient :: Float,
  playerHp     :: Int,
  playerSpeed  :: Float,
  playerFr     :: Float,
  playerHbox   :: Point,
  playerAnim   :: Animation
} deriving Eq

data PlayerBullet = PlayerBullet {
  pbPos        :: Point,
  pbOrient     :: Float,
  pbDmg        :: Int,
  pbSpeed      :: Float,
  pbHbox       :: Point
} deriving Eq

-- data Enemy = Enemy {
--   enemyPos     :: Point,
--   enemyOrient :: Float,
--   enemyHp     :: Int,
--   enemySpeed  :: Float,
--   enemyFr     :: Float,
--   enemyHbox   :: Point
-- }

data Obstacle = Obstacle {
  obstaclePos    :: Point,
  obstacleOrient :: Float,
  obstacleSpeed  :: Float,
  obstacleHp     :: Int,
  obstacleHbox   :: Point
} deriving Eq

-- Enemy data type
-- Various bullet types
-- Power up data types
  -- data PowerUpType
  --   = FireRateIncrease
  --   | HealthIncrease
  --   | SpeedIncrease -- Increases movement speed
  --   | Invincibility


-- | Positionable type class
class Positionable a where
  getPosition :: a -> Point
  getOrientation :: a -> Float
  changePosition :: a -> Point -> a
  -- changeOrientation :: a -> Float -> a

instance Positionable Player where
  getPosition Player{playerPos} = playerPos
  getOrientation Player{playerOrient} = playerOrient
  changePosition p newPos = p{playerPos = newPos}

instance Positionable PlayerBullet where
  getPosition PlayerBullet{pbPos} = pbPos
  getOrientation PlayerBullet{pbOrient} = pbOrient
  changePosition pb newPos = pb{pbPos = newPos}

instance Positionable Obstacle where
  getPosition Obstacle{obstaclePos} = obstaclePos
  getOrientation Obstacle{obstacleOrient} = obstacleOrient
  changePosition o newPos = o{obstaclePos = newPos}

instance Positionable Explosion where
  getPosition Explosion{explosionPos} = explosionPos
  getOrientation Explosion{explosionOrient} = explosionOrient
  changePosition e newPos = e{explosionPos = newPos}

-- | Drawable type class
class Positionable a => Drawable a where
  getSprite :: Sprites -> a -> Picture
  toPicture :: Sprites -> a -> Picture
  toPicture sprites x = draw (getOrientation x) (getPosition x) (getSprite sprites x)

instance Drawable Player where
  getSprite Sprites{playerSprites} Player{playerAnim = Anim index _} = playerSprites !! index

instance Drawable PlayerBullet where
  getSprite Sprites{pBulletSprite} _ = pBulletSprite

instance Drawable Obstacle where
  getSprite Sprites{obstacleSprite} _ = obstacleSprite

instance Drawable Explosion where
  getSprite Sprites{explosionSprites} Explosion{explosionAnim = Anim index _} = explosionSprites !! index

draw :: Float -> Point -> Picture -> Picture
draw orientation (x,y) = rotate orientation . translate x y

-- | Collideable type class
class Collideable a where
  getHitbox :: a -> Point

instance Collideable Player where
  getHitbox Player{playerHbox} = playerHbox 

instance Collideable PlayerBullet where
  getHitbox PlayerBullet{pbHbox} = pbHbox

instance Collideable Obstacle where
  getHitbox Obstacle{obstacleHbox} = obstacleHbox

-- | Destructible type class
class (Positionable a, Collideable a, Eq a) => Destructible a where
  applyDamage :: a -> Int -> (Bool, a) -- For the Bool value: True means alive, False means dead
  destroy :: a -> GameState -> GameState
  update :: Int -> a -> GameState -> GameState

instance Destructible Player where
  applyDamage player@Player {playerHp} damage 
    | playerHp - damage <= 0 = (False, player{playerHp = playerHp - damage})
    | otherwise = (True, player{playerHp = playerHp - damage})
  destroy p gstate = undefined
  update _ p gstate = gstate{player = p}

instance Destructible Obstacle where
  applyDamage obs@Obstacle{obstacleHp} damage
    | obstacleHp - damage <= 0 = (False, obs)
    | otherwise = (True, obs {obstacleHp = obstacleHp - damage})
  destroy o gstate@GameState{obstacles} = gstate{obstacles = delete o obstacles}
  update i o gstate@GameState{obstacles} = gstate{obstacles = ((o :) . deleteAt i) obstacles}

instance Destructible PlayerBullet where
  applyDamage obs damage = undefined
  destroy pb gstate@GameState{playerBullets} = gstate{playerBullets = delete pb playerBullets}
  update i pb gstate@GameState{playerBullets} = gstate{playerBullets = ((pb :) . deleteAt i) playerBullets}

-- | Moveable type class
class Positionable a => Moveable a where
  getSpeed :: a -> Float
  move :: a -> a
  move a = changePosition a (x + (speed * cos orient), y + (speed * sin orient)) where
    (x,y) = getPosition a
    orient = getOrientation a
    speed = getSpeed a

instance Moveable PlayerBullet where
  getSpeed PlayerBullet{pbSpeed} = pbSpeed

instance Moveable Obstacle where
  getSpeed Obstacle {obstacleSpeed} = obstacleSpeed

-- | Shootable type class
class (Positionable a, Collideable a) => Shootable a where
  shoot :: Destructible b => a -> [b] -> (HitInfo, Maybe b)

data HitInfo = Miss | Damage Int | Kill

instance Shootable PlayerBullet where
  shoot pb@PlayerBullet{pbDmg} xs = case find (collide pb) xs of
    Just x -> case applyDamage x pbDmg of
      (True, y)  -> (Damage i, Just y)
      (False, y) -> (Kill, Just y)
      where (Just i) = elemIndex x xs
    Nothing -> (Miss, Nothing)
    
    
{-    __        __
     /\ \      /\ \       [ ]    _   _     _______
    /  \ \    /  \ \       _    | | / /   |
   / /\ \ \  / /\ \ \     | |   | |/ /    |
  / / /\ \ \/ / /\ \ \    | |   | | /     |_______
 / / /  \_\/ / /  \_\ \   | |   | |\      |
/ / /    \_\/_/    \_\_\  | |   | | \     |
\/_/                \/_/  |_|   |_|  \_   |_______
-}

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

-- TO DO : move these helper functions to seperate file or something
clamp :: Float -> (Float, Float) -> Float
clamp x (l,u) = max (min x u) l

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = l ++ r
  where (l, _:r) = splitAt i xs



-- stuff for enemy spawning
-- hlint be angery when i put data so yea we have newtype now
newtype EnemyList = EnemyList {enemies :: [EnemyListEnemy]} deriving (Show, Generic)

data EnemyListEnemy = EnemyListEnemy
  { eleTime :: Float,
    eleType :: String
  }
  deriving (Show, Generic)

instance FromJSON EnemyList
instance FromJSON EnemyListEnemy