-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | timeElapsed gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO :: IO Integer
       let newNumber = abs randomNumber `mod` 10
       return $ initialState -- GameState (ShowANumber newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate {timeElapsed = timeElapsed gstate + secs}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = movePlayer (world gstate) where
  movePlayer (World (x, y)) = case c of
    'w' -> gstate { world = World (x, y + 10)}
    's' -> gstate { world = World (x, y - 10)}
    _   -> gstate
inputKey _ gstate = gstate