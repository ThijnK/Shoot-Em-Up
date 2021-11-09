{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller ( input, step )
import View ( view )
import Defaults ( initialState )
import SaveLoad ( loadSpawnList, loadSprites )

import Graphics.Gloss ( black, Display(InWindow) )
import Graphics.Gloss.Interface.IO.Game ( playIO )
import System.Random ( getStdGen )

main :: IO ()
main = do sprites     <- loadSprites
          enemyList   <- loadSpawnList
          generator   <- getStdGen
          playIO (InWindow "Shoot-Em-Up by Thijn Kroon & Mike Wu" (1000, 600) (0, 0)) -- Or FullScreen
            black            -- Background color
            60               -- Frames per second
            (initialState sprites enemyList generator) -- Initial state
            view             -- View function
            input            -- Event function
            step             -- Step function