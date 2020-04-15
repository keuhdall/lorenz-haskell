module Main where

import Graphics.Gloss
import Control.Monad.State (State, execState, get)
import Control.Lens

import Attractor.Data
import Attractor.State
import Controls
import Draw
import Window

updateValues :: State Attractor ()
updateValues = do
  l <- get
  let (dx,dy,dz) = l^.equation $ l
  x += dx
  y += dy
  z += dz
  points .= convert l:l^.points

update :: Float -> Attractor -> Attractor
update _ = execState $ do
  updateValues
  mustChangeAttractor

render :: Attractor -> Picture
render l = color white . line $ l^.points

main :: IO ()
main = play window bgColor fps (initialState Lorenz) render handleKeys update