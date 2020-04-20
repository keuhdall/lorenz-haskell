module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Control.Monad.State (State, execState, get)
import Control.Lens

import Attractor.Data
import Attractor.State
import Controls.Events
import Controls.Keyboard
import Controls.Mouse
import Draw
import Window

updateValues :: State Attractor ()
updateValues = do
  att <- get
  let (dx,dy,dz) = att^.equation $ att
  x += dx
  y += dy
  z += dz
  points .= convert att:att^.points

update :: Float -> Attractor -> Attractor
update _ = execState $ do
  updateValues
  mustChangeAttractor
  mustUpdateAttractorPos

update' :: ViewPort -> Float -> Attractor -> Attractor
update' _ _ = execState $ do
  updateValues
  mustChangeAttractor
  mustUpdateAttractorPos

render :: Attractor -> Picture
render att = color white . line $ att^.points

main :: IO ()
--main = simulate window bgColor fps (initialState NoseHoover) render update'
main = play window bgColor fps (initialState Lorenz) render handleEvents update