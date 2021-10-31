module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do sprites <- loadSprites
          playIO (InWindow "Shoot-Em-Up by Thijn Kroon & Mike Wu" (1000, 600) (0, 0)) -- Or FullScreen
            black            -- Background color
            60               -- Frames per second
            (initialState sprites)  -- Initial state
            view             -- View function
            input            -- Event function
            step             -- Step function

loadSprites :: IO Sprites
loadSprites = do player      <- loadBMP "assets/player.bmp"
                 bullet      <- loadBMP "assets/laser.bmp"
                 obstacle    <- loadBMP "assets/obstacle.bmp"
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
                 let explosions = [explosion1, explosion2, explosion3, explosion4, explosion5, explosion6, explosion7, explosion8, explosion9, explosion10, explosion11]
                 return (Sprites player bullet obstacle explosions)