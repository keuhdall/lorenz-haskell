module Attractor.State where

import qualified Data.Set as S

import Attractor.Data
import Attractor.Equations
import Window

initialState :: AttractorType -> Attractor
initialState t = Attractor {
  _x = 0.1,
  _y = 0.1,
  _z = 0.1,
  _d = screenDistance,
  _attr = t,
  _points = [],
  _keys = S.empty,
  _equation = getEquation t
}