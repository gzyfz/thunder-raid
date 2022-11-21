{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Board

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of
  AppEvent Tick                   -> Brick.continue (updateAll s)
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (generate Bullet s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (generate Enemy s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psBoard = update oldpos f (psBoard s),
               playerPos = newpos }
            where oldpos = playerPos s
                  newpos = f oldpos

-------------------------------------------------------------------------------
generate :: Piece -> PlayState -> PlayState
-------------------------------------------------------------------------------
generate piece s 
  | piece == Bullet = s { psBoard = put (Just Bullet) (Pos 2 x)  (psBoard s) }
  | piece == Enemy  = s { psBoard = put (Just Enemy)  (Pos 10 x) (psBoard s) }
  where x = pCol (playerPos s)

-------------------------------------------------------------------------------
updateAll :: PlayState -> PlayState
-------------------------------------------------------------------------------
updateAll s = s { psBoard = refreshAll (psBoard s)}