module Attractor.Equations where

import Control.Lens

import Attractor.Data

lorenzEquation :: SystemEquation
lorenzEquation l = (dx, dy, dz) where
  dx = (θ    * (l^.y  - l^.x)       ) * δt
  dy = (l^.x * (ρ     - l^.z) - l^.y) * δt
  dz = (l^.x *  l^.y  - ß     * l^.z) * δt
  θ = 10
  ρ = 28
  ß = 8.0 / 3.0

rosslerEquation :: SystemEquation
rosslerEquation l = (dx, dy, dz) where
  dx = -(l^.y - l^.z              ) * δt
  dy =  (l^.x + a     * l^.y      ) * δt
  dz =  (b    + l^.z  * (l^.x - c)) * δt
  a = 0.2
  b = 0.2
  c = 5.7

rikitakeEquation :: SystemEquation
rikitakeEquation l = (dx, dy, dz) where
  dx = -µ * l^.x + l^.z * l^.y * δt
  dy = -µ * l^.y + l^.x * (l^.z - a) * δt
  dz = 1 - l^.x * l^.y * δt
  µ = 1.0
  a = 5.0

noseHooverEquation :: SystemEquation
noseHooverEquation l = (dx, dy, dz) where
  dx = l^.y * δt
  dy = l^.y * l^.z - l^.x * δt
  dz = 1 - (l^.y) ** 2 * δt

wangEquation :: SystemEquation
wangEquation l = (dx, dy, dz) where
  dx = l^.x - l^.y * l^.z * δt
  dy = l^.x - l^.y + l^.x * l^.z * δt
  dz = -3 * l^.z + l^.x * l^.y * δt

duffingEquation :: SystemEquation
duffingEquation l = (dx, dy, dz) where
  dx = l^.y * δt
  dy = -a * l^.y - (l^.x) ** 3 + b * cos(l^.z) * δt
  dz = 1 * δt
  a = 0.1
  b = 11.0

lotkaVolterraEquation :: SystemEquation
lotkaVolterraEquation a = (dx, dy, dz) where
  dx = a^.x * (1 - a^.x - 9 * a^.y) * δt
  dy = -a^.y * (1 - 6*a^.x - a^.y + 9 * a^.z) * δt
  dz = a^.z * (1 - 3 * a^.x - a^.z) * δt

chuaEquation :: SystemEquation
chuaEquation a = (dx, dy, dz) where
  dx = α * (a^.y - a^.x - ht) * δt
  dy = a^.x - a^.y + a^.z * δt
  dz = -ß * a^.y * δt
  ht = µ1 * a^.x + 0.5 * (µ0 - µ1) * ((abs $ a^.x + 1) - (abs $ a^.x - 1))
  α = 15.6
  ß = 28.0
  µ0 = -1.143
  µ1 = -0.714

getEquation :: AttractorType -> SystemEquation
getEquation eq = case eq of
  Lorenz        -> lorenzEquation
  Rossler       -> rosslerEquation
  Rikitake      -> rikitakeEquation
  NoseHoover    -> noseHooverEquation
  Wang          -> wangEquation
  Duffing       -> duffingEquation
  LotkaVolterra -> lotkaVolterraEquation
  Chua          -> chuaEquation
