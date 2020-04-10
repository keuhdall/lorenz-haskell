{-# LANGUAGE TemplateHaskell #-}

module Lorenz where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Control.Lens
import Control.Monad.State

θ :: Float
θ = 10

ρ :: Float
ρ = 28

ß :: Float
ß = 8.0 / 3.0

δt :: Float
δt = 0.005

data Lorenz = Lorenz {
  _x :: Float,
  _y :: Float,
  _z :: Float,
  _d :: Float,
  _points :: Path
}
makeLenses ''Lorenz

derivatives :: Lorenz -> (Float, Float, Float)
derivatives l = (dx, dy, dz) where
  dx = (θ    * (l^.y  - l^.x)       ) * δt
  dy = (l^.x * (ρ     - l^.z) - l^.y) * δt
  dz = (l^.x *  l^.y  - ß     * l^.z) * δt