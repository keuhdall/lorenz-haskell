module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Control.Monad.State (State, execState, get)
import Control.Lens

import Window
import Lorenz
import Draw

initialState :: Lorenz
initialState = Lorenz {
  _x = 0.1,
  _y = 0.1,
  _z = 0.1,
  _d = screenDistance,
  _points = []
}

updateValues :: State Lorenz ()
updateValues = do
  l <- get
  let (dx,dy,dz) = derivatives l
  x += dx
  y += dy
  z += dz
  points .= convert l:l^.points

update :: ViewPort -> Float -> Lorenz -> Lorenz
update vp t l = execState updateValues l

render :: Lorenz -> Picture
render l = color white . line $ l^.points

main :: IO ()
main = simulate window bgColor fps initialState render update
