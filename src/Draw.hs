module Draw where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Control.Lens

import Lorenz

pointRadius :: Float
pointRadius = 0.1

pointPic :: Picture
pointPic = color white $ circleSolid pointRadius

convert :: Lorenz -> (Float, Float)
convert l = (l^.d / l^.z * l^.x, l^.d / l^.z * l^.y)