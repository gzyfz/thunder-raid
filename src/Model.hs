{-# LANGUAGE RecordWildCards #-}
module Model where

import Prelude hiding ((!!))
import Board

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State
  = Intro
  | Play PlayState
  | Outro

data PlayState = PS
  { psBoard  :: Board.Board,
    playerPos :: Board.Pos
  }

init :: PlayState
init = PS
  { psBoard  = Board.init,
    playerPos = Pos 1 8
  }
