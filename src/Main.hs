{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where


import Brick ( customMain, App(..) )
import Graphics.Vty.Attributes ( defAttr,withForeColor )
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model ( PlayState, Tick(..), init )
import View ( view, enemyAttr, playerAttr, explosionAttr) 
import Control ( control ) 
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Brick.AttrMap ( attrMap, AttrMap )

-------------------------------------------------------------------------------
main :: IO ()
main = do
  rounds <- fromMaybe defaultRounds <$> getRounds
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <-customMain  initialVty buildVty (Just chan) app (Model.init)
  print "finish"

app :: App PlayState Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const (theMap)
  }

getRounds :: IO (Maybe Int)
getRounds = do
  args <- getArgs
  case args of
    (str:_) -> return (readMaybe str)
    _       -> return Nothing

defaultRounds :: Int
defaultRounds = 3


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ 
  (enemyAttr , defAttr `withForeColor` V.brightRed ),
  (playerAttr , defAttr `withForeColor` V.cyan ),
  (explosionAttr , defAttr `withForeColor` V.yellow )
  ]

