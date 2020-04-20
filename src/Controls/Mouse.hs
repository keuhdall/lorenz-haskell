module Controls.Mouse (mustUpdateAttractorPos) where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Control.Lens

import Attractor.Data

updateAttractorPos :: State Attractor ()
updateAttractorPos = do
  att <- get
  let (a,b) = att^.pos
  let updatePos (a',b') = (a'+a,b'+b)
  points %= map updatePos
  return ()

mustUpdateAttractorPos :: State Attractor ()
mustUpdateAttractorPos = do
  att <- get
  if att^.mouse.contains LeftButton then updateAttractorPos else return ()