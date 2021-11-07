{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Contains functionality for saving and loading gamestate and enemyList to/from files
module SaveLoad where

import Model

import Data.Aeson
import Graphics.Gloss ( Picture(Blank) )
import Graphics.Gloss.Data.Bitmap ( loadBMP )
import System.Random ( mkStdGen, StdGen )
import qualified Data.ByteString.Lazy as BS

-- | Functions for saving/loading things from files

-- Save the game to a JSON file
saveGame :: GameState -> IO GameState
saveGame gstate = do let gstate' = gstate{saveLoad = (False, False)}
                     BS.writeFile "game/savegame.json" (encode gstate')
                     print "Game saved!"
                     return gstate'

-- Load the game from a JSON file
loadGame :: GameState -> IO GameState
loadGame gstate@GameState{sprites, generator}
  = do loadedFile <- BS.readFile "game/savegame.json"
       let gstate' = case decode loadedFile of
                        Nothing -> gstate
                        Just gs -> gs{paused = True, sprites = sprites, generator = generator, downKeys = []}
       print "Game loaded!"
       return gstate'{saveLoad = (False, False)}

-- Load enemy list from JSON file
loadEnemyList :: IO EnemyList
loadEnemyList = do enemyList <- BS.readFile "game/enemies.json"
                   case decode enemyList of
                     Nothing  -> return (EnemyList []) -- load failed, return empty list
                     Just any -> return any

-- Load sprites from .bmp files
loadSprites :: IO Sprites
loadSprites = do player1     <- loadBMP "assets/player-1.bmp"
                 player2     <- loadBMP "assets/player-2.bmp"
                 player3     <- loadBMP "assets/player-3.bmp"
                 player4     <- loadBMP "assets/player-4.bmp"
                 player5     <- loadBMP "assets/player-5.bmp"
                 turret1     <- loadBMP "assets/turret-1.bmp"
                 turret2     <- loadBMP "assets/turret-2.bmp"
                 turret3     <- loadBMP "assets/turret-3.bmp"
                 turret4     <- loadBMP "assets/turret-4.bmp"
                 drone1      <- loadBMP "assets/drone-1.bmp"
                 drone2      <- loadBMP "assets/drone-2.bmp"
                 drone3      <- loadBMP "assets/drone-3.bmp"
                 kamikaze    <- loadBMP "assets/kamikaze.bmp"
                 bullet1     <- loadBMP "assets/bullet-1.bmp"
                 bullet2     <- loadBMP "assets/bullet-2.bmp"
                 meteor      <- loadBMP "assets/meteor.bmp" -- credits to AX Assets: https://axassets.itch.io/spaceship-simple-assets
                 explosion1  <- loadBMP "assets/explosion-01.bmp"
                 explosion2  <- loadBMP "assets/explosion-02.bmp"
                 explosion3  <- loadBMP "assets/explosion-03.bmp"
                 explosion4  <- loadBMP "assets/explosion-04.bmp"
                 explosion5  <- loadBMP "assets/explosion-05.bmp"
                 explosion6  <- loadBMP "assets/explosion-06.bmp"
                 explosion7  <- loadBMP "assets/explosion-07.bmp"
                 explosion8  <- loadBMP "assets/explosion-08.bmp"
                 explosion9  <- loadBMP "assets/explosion-09.bmp"
                 explosion10 <- loadBMP "assets/explosion-10.bmp"
                 explosion11 <- loadBMP "assets/explosion-11.bmp"
                 let player = [player1, player2, player3, player2, player1, player4, player5, player4]
                 let turret = [turret1, turret2, turret3, turret4]
                 let drone = [drone1, drone2, drone3, drone2]
                 let explosion = [explosion1, explosion2, explosion3, explosion4, explosion5, explosion6, explosion7, explosion8, explosion9, explosion10, explosion11]
                 return (Sprites player bullet1 bullet2 meteor turret drone kamikaze explosion)

-- | Necessary instances for encoding and decoding JSON

-- Because everything is generic, we can just let aeson handle the encoding
instance ToJSON GameState

-- Help Aeson by instancing ToJSON for generics
instance ToJSON Score
instance ToJSON Player
instance ToJSON Turret
instance ToJSON Drone
instance ToJSON Kamikaze
instance ToJSON FireRate
instance ToJSON PlayerBullet
instance ToJSON EnemyBullet
instance ToJSON Meteor
instance ToJSON Explosion
instance ToJSON Animation
instance ToJSON Sprites where
  toJSON sprites = object []
instance ToJSON EnemyList
instance ToJSON EnemyListEnemy
instance ToJSON StdGen where
  toJSON stdgen = object []

-- Construct GameState from JSON
instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \v ->
    GameState
      <$> v .: "score"
      <*> v .: "paused"
      <*> v .: "gameOver"
      <*> v .: "deltaTime"
      <*> v .: "timeElapsed"
      <*> v .: "downKeys"
      <*> v .: "saveLoad"
      <*> v .: "player"
      <*> v .: "turrets"
      <*> v .: "drones"
      <*> v .: "kamikazes"
      <*> v .: "playerBullets"
      <*> v .: "enemyBullets"
      <*> v .: "meteors"
      <*> v .: "explosions"
      <*> v .: "sprites"
      <*> v .: "enemyList"
      <*> v .: "generator"

instance FromJSON Score
instance FromJSON Player
instance FromJSON FireRate
instance FromJSON Animation
instance FromJSON Turret
instance FromJSON Drone
instance FromJSON Kamikaze
instance FromJSON PlayerBullet
instance FromJSON EnemyBullet
instance FromJSON Meteor
instance FromJSON Explosion

instance FromJSON Sprites where
  parseJSON = withObject "Sprites" $ \obj -> do
    return (Sprites {playerSprites = [Blank], pBulletSprite = Blank, eBulletSprite = Blank, meteorSprite = Blank, turretSprites = [Blank], droneSprites = [Blank], kamikazeSprite = Blank, explosionSprites = [Blank]})
  
instance FromJSON StdGen where
  parseJSON = withObject "StdGen" $ \obj -> do
    return (mkStdGen 69)

-- Instances for loading EnemyList from JSON file
instance FromJSON EnemyList
instance FromJSON EnemyListEnemy