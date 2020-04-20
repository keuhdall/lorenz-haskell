{-# LANGUAGE TemplateHaskell #-}

module Attractor.Data where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import qualified Data.Set as S

δt :: Float
δt = 0.005

data AttractorType =
  Lorenz
  | Rossler
  | Rikitake
  | NoseHoover
  | Wang
  | Duffing
  | LotkaVolterra
  | Chua

type SystemEquation = Attractor -> (Float, Float, Float)

data Attractor = Attractor {
  _x, _y, _z, _d  :: Float,
  _pos            :: Point,
  _attr           :: AttractorType,
  _points         :: Path,
  _keys           :: S.Set Key,
  _mouse          :: S.Set MouseButton,
  _equation       :: SystemEquation
}
makeLenses ''Attractor