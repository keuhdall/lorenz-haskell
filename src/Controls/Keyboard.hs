module Controls.Keyboard (mustChangeAttractor) where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State
import Control.Lens
import Data.Maybe

import Attractor.Data
import Attractor.State

attractorFromKey :: Char -> AttractorType
attractorFromKey c = case c of
  '1' -> Lorenz
  '2' -> Rossler
  '3' -> Rikitake
  '4' -> NoseHoover
  '5' -> Wang
  '6' -> Duffing
  '7' -> LotkaVolterra
  '8' -> Chua
  _   -> Lorenz

mustChangeAttractor :: State Attractor ()
mustChangeAttractor = do
  a <- get
  let keyPressed key = a^.keys.contains (Char key)
  let newAttractor = attractorFromKey <$> (listToMaybe $ filter keyPressed ['1'..'8'])
  case newAttractor of
    Just newAttr  -> put $ initialState newAttr
    Nothing       -> return ()
