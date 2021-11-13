-- Contains helper functions used in multiple places in the code
module Helper where

import Model ( Animation(..), FireRate(..) )

import Graphics.Gloss(Point, Picture, translate, rotate)
import Graphics.Gloss.Geometry.Angle ( radToDeg )

-- Replace the element at the given index of the list with the given value
replace :: Int -> a -> [a] -> [a]
replace index x xs = zs ++ (x:ys)
  where (zs, _:ys) = splitAt index xs

-- Clamps a given value to the given bounds (lowerBound, upperBound)
clamp :: Float -> (Float, Float) -> Float
clamp x (l,u) = max (min x u) l

-- Updates firerate
updateFr :: Float -> FireRate -> FireRate
updateFr secs (FireRate fr last)
  | last + secs > fr = FireRate fr 0
  | otherwise        = FireRate fr (last + secs)

-- Repeats animation when it reaches the end
animateR :: Float -> Animation -> Animation
animateR secs (Animation index total speed last)
  | last + secs > speed = Animation ((index + 1) `mod` total) total speed 0
  | otherwise = Animation index total speed (last + secs)

-- Returns Nothing when animation reaches the end
animateM :: Float -> Animation -> Maybe Animation
animateM secs (Animation index total speed last)
  | index + 1 > total   = Nothing
  | last + secs > speed = Just (Animation (index + 1) total speed 0)
  | otherwise           = Just (Animation index total speed (last + secs))

-- Calculate the angle between two points
anglePoints :: Point -> Point -> Float
anglePoints (x1,y1) (x2,y2) = atan2 (y2 - y1) (x2 - x1)

-- Clamps the orientation to facing the the left side of the screen
clampOrientation :: Float -> Float
clampOrientation angle
  | angle > -0.75 * pi && angle < 0 = -0.75 * pi
  | angle < 0.75 * pi && angle > 0  = 0.75 * pi
  | otherwise                       = angle

draw :: Float -> Point -> Picture -> Picture
draw orientation (x,y) = translate x y . rotate (radToDeg (-orientation))