module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
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
render l = pictures $ translatePoint <$> l^.points where
  translatePoint (x,y) = translate (w/2) (h/2) $ translate x y pointPic
  (w,h) = (fromIntegral . fst $ windowSize, fromIntegral . snd $ windowSize)

main :: IO ()
main = simulate window bgColor fps initialState render update
