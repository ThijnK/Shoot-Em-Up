module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = 
    do 
        playerImg <- loadBMP "assets/Blue-5.bmp"
        playerBullet <- loadBMP "assets/laser.bmp"
        let sprites = Sprites playerImg playerBullet
        playIO (InWindow "Shoot-Em-Up by Thijn Kroon & Mike Wu" (1000, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState sprites)  -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
            
-- data type for all images...