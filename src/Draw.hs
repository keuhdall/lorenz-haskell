module Draw (pointPic, convert) where

import Graphics.Gloss
import Control.Lens

import Attractor.Data

pointRadius :: Float
pointRadius = 1

pointPic :: Picture
pointPic = color white $ circleSolid pointRadius

convert :: Attractor -> (Float, Float)
convert l = (l^.x * l^.d / l^.z, l^.y * l^.d / l^.z)