module Attractor.Equations where

import Control.Lens

import Attractor.Data

lorenzEquation :: SystemEquation
lorenzEquation l = (dx*δt, dy*δt, dz*δt) where
  dx = θ    * (l^.y  - l^.x)       
  dy = l^.x * (ρ     - l^.z) - l^.y
  dz = l^.x *  l^.y  - ß     * l^.z
  θ = 10
  ρ = 28
  ß = 8.0 / 3.0

rosslerEquation :: SystemEquation
rosslerEquation l = (dx*δt, dy*δt, dz*δt) where
  dx = -(l^.y - l^.z)
  dy =  l^.x  + a     * l^.y
  dz =  b     + l^.z  * (l^.x - c)
  a = 0.2
  b = 0.2
  c = 5.7

rikitakeEquation :: SystemEquation
rikitakeEquation l = (dx*δt, dy*δt, dz*δt) where
  dx = -µ * l^.x + l^.z * l^.y
  dy = -µ * l^.y + l^.x * (l^.z - a)
  dz = 1  - l^.x * l^.y
  µ = 1.0
  a = 5.0

noseHooverEquation :: SystemEquation
noseHooverEquation l = (dx*δt, dy*δt, dz*δt) where
  dx =  l^.y
  dy = -l^.x + l^.y * l^.z
  dz =  1  * ((l^.y) ** 2)

wangEquation :: SystemEquation
wangEquation l = (dx*δt, dy*δt, dz*δt) where
  dx = l^.x - l^.y * l^.z
  dy = l^.x - l^.y + l^.x * l^.z
  dz = -3   * l^.z + l^.x * l^.y

duffingEquation :: SystemEquation
duffingEquation l = (dx*δt, dy*δt, dz*δt) where
  dx = l^.y
  dy = -a * l^.y - (l^.x) ** 3 + b * cos(l^.z)
  dz = 1
  a = 0.1
  b = 11.0

lotkaVolterraEquation :: SystemEquation
lotkaVolterraEquation a = (dx*δt, dy*δt, dz*δt) where
  dx =  a^.x * ( 1 - 1 * a^.x - 9 * a^.y         )
  dy = -a^.y * ( 1 - 6 * a^.x - a^.y + 9 * a^.z  )
  dz =  a^.z * ( 1 - 3 * a^.x - a^.z             )

chuaEquation :: SystemEquation
chuaEquation a = (dx*δt, dy*δt, dz*δt) where
  dx = α * (a^.y - a^.x - ht)
  dy = a^.x - a^.y + a^.z
  dz = -ß * a^.y
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
