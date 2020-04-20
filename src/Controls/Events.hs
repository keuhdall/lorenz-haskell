module Controls.Events (handleEvents) where

import Graphics.Gloss.Interface.Pure.Game
import Control.Lens

import Attractor.Data

updatePos :: Point -> Point -> Point
updatePos (px',py') (px,py) =
  let px'' = if px' < px then (-1) else if px' > px then 1 else 0;
      py'' = if py' < py then (-1) else if py' > py then 1 else 0
    in (px'',py'')


handleEvents :: Event -> Attractor -> Attractor
handleEvents (EventKey (MouseButton m) s _ _) att = mouse . contains m .~ (s == Down) $ att
handleEvents (EventKey k               s _ _) att = keys  . contains k .~ (s == Down) $ att
handleEvents (EventMotion p) att = pos %~ updatePos p $ att
handleEvents _ att = att
