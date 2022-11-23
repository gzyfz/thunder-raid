{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Control where

import Brick ( EventM, Next, BrickEvent(AppEvent), halt, continue)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import System.Random ( getStdRandom, Random(randomR) )
import System.CPUTime ()
import Model ( PlayState(playerPos, psBoard, playerScore, playerTime), Tick(..) )
import Board
    ( Pos(Pos, pCol),
      Piece(Enemy, Bullet),
      put,
      update,
      refreshAll,
      left,
      right,
      sumExplosion,
      getPlayerPos )

-------------------------------------------------------------------------------
hardLevel :: Int
hardLevel = 100

totalTime:: Int
totalTime = 100

unitTime:: Int
unitTime = 10


control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)

control s ev  
  | (playerTime  s)  == totalTime * unitTime = Brick.halt s
  | (playerPos s) == (Pos 0 0)               = Brick.halt s
  | otherwise  =   case ev of
  AppEvent Tick                   ->
    do
      ri <- getStdRandom $ randomR (0, 150 :: Int)
      if ri < hardLevel then Brick.continue (updateAllAndAddEnemy s ri)
      else Brick.continue (updateAllOnly s)
  T.VtyEvent (V.EvKey V.KUp   _)  ->
    Brick.continue (generate Bullet x s)
    where x = pCol (playerPos s)
  T.VtyEvent (V.EvKey V.KDown _)  ->
    do
      ri <- getStdRandom $ randomR (0, 15 :: Int)
      Brick.continue (generate Enemy ri s)
  -- ri denotes random integer

  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)

  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psBoard = newboard, 
               playerPos = newpos }
            where oldpos = playerPos s
                  newboard = update oldpos f (psBoard s)
                  newpos = getPlayerPos newboard

-------------------------------------------------------------------------------
generate :: Piece -> Int -> PlayState -> PlayState
-------------------------------------------------------------------------------

generate piece x s
  | piece == Bullet = s { psBoard = put (Just Bullet) (Pos 2 x)  (psBoard s) }
  | piece == Enemy  = s { psBoard = put (Just Enemy)  (Pos 10 x) (psBoard s) }

-------------------------------------------------------------------------------
-- updateAll :: PlayState -> PlayState
-------------------------------------------------------------------------------

updateAllAndAddEnemy s x  = s { playerTime =  time + 1,
                                psBoard = newboard,
                                playerScore = score + (sumExplosion newboard),
                                playerPos = getPlayerPos newboard }
                            where time = playerTime s
                                  score = playerScore s
                                  newboard = refreshAll (psBoard (generate Enemy x s))


updateAllOnly s = s { playerTime =  time + 1,
                      psBoard = newboard,
                      playerScore = score + (sumExplosion newboard),
                      playerPos = getPlayerPos newboard }
                      where time = playerTime s
                            score = playerScore s
                            newboard = refreshAll (psBoard s)

-- randomly choose whether to refresh the screen or generate a new enemy brfore refreshing


--  s { psBoard = refreshAll (psBoard s)}
