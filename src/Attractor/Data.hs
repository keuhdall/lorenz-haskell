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
  _x :: Float,
  _y :: Float,
  _z :: Float,
  _d :: Float,
  _attr :: AttractorType,
  _points :: Path,
  _keys :: S.Set Key,
  _equation :: SystemEquation
}
makeLenses ''Attractor