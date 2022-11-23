{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where


import Brick ( customMain, App(..) )
import Graphics.Vty.Attributes ( defAttr,withForeColor )
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)
import Text.Printf (printf)
import Model ( PlayState(playerScore), Tick(..), init )
import View ( view, enemyAttr, playerAttr, explosionAttr) 
import Control ( control ) 
import Brick.AttrMap ( attrMap, AttrMap )

-------------------------------------------------------------------------------
main :: IO ()
main = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <-customMain  initialVty buildVty (Just chan) app (Model.init)
  printf "Your score is %d\n" (playerScore res)

app :: App PlayState Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const (theMap)
  }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ 
  (enemyAttr , defAttr `withForeColor` V.brightRed ),
  (playerAttr , defAttr `withForeColor` V.cyan ),
  (explosionAttr , defAttr `withForeColor` V.yellow )
  ]
