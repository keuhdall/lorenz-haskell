module Draw (pointPic, convert) where

import Graphics.Gloss
import Control.Lens

import Attractor.Data

pointRadius :: Float
pointRadius = 1

pointPic :: Picture
pointPic = color white $ circleSolid pointRadius

convert :: Attractor -> (Float, Float)
convert att = (att^.x * att^.d / att^.z, att^.y * att^.d / att^.z)