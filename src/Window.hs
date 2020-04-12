module Window where

import Graphics.Gloss

windowName :: String
windowName = "Lorenz"

windowSize :: (Int, Int)
windowSize = (800, 600)

windowPos :: (Int, Int)
windowPos = (10, 10)

bgColor :: Color
bgColor = black

fps :: Int
fps = 300

screenDistance :: Float
screenDistance = 200.0

window :: Display
window = InWindow windowName windowSize windowPos