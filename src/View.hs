module View (view,enemyAttr,playerAttr) where

import Brick
import Brick.Widgets.Center (center, hCenterLayer)
import Brick.Widgets.Border (borderWithLabel, border)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model ( PlayState(playerPos, psBoard, playerTime) )
import Board ( (!), dimX, dimY, Piece(..), Pos(Pos) )
import Graphics.Vty()
import Control (totalTime, unitTime)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s <=> drawTimeBox s]

view' :: PlayState -> Widget String
view' s =
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile (reverseList [ mkRow s row | row <- [1..dimY] ])
      where
        reverseList [] = []
        reverseList (x:xs) = reverseList xs ++ [x]

header :: PlayState -> String
header _ = printf "Thunder Raid"

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dimX] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c
  | playerPos s == Pos r c                 = center (mkPiece (Just Player))
  | (psBoard s) ! (Pos r c) == Just Enemy  = center (mkPiece (Just Enemy))
  | (psBoard s) ! (Pos r c) == Just Bullet = center (mkPiece (Just Bullet))
  | otherwise                              = center (mkPiece Nothing)

mkPiece :: Maybe Piece -> Widget n
mkPiece Nothing  = block_none
mkPiece (Just Player) = blockPlayer
mkPiece (Just Enemy) = blockEnemy 
mkPiece (Just Bullet) = blockBullet

block_none, block_enemy, block_player :: Widget n
block_none   = vBox (replicate 5 (str "         "))
block_enemy  = vBox [ str "    _    ",
                      str " <  |  > ",
                      str "   | |   ",
                      str "    V    ",
                      str "         " ]
block_player = vBox [ str "    ^    ",
                      str "   |||   ",
                      str "  ^ | ^  ",
                      str "<| | | |>",
                      str " _* ^ *_ "]
block_bullet = vBox [ str "         ",
                      str "    ^    ",
                      str "   |||   ",
                      str "   ***    ",
                      str "         "]





vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : bs)
vTile _      = emptyWidget



drawTimeBox ::PlayState -> Widget n
drawTimeBox s =  hCenterLayer (str "Time Left : " <+> 
                      border (hBox (replicate (min totalTime (timeOut (playerTime s))) (str ">") 
                      ++ replicate (max 0 (totalTime-timeOut (playerTime s)) ) (str " ")  )))


timeOut :: Int -> Int
timeOut t
  | t == 0 = 0
  | otherwise = t  `div` unitTime

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : bs)
hTile _      = emptyWidget


blockEnemy :: Widget n
blockEnemy = withAttr enemyAttr block_enemy


blockPlayer :: Widget n
blockPlayer = withAttr playerAttr block_player

blockBullet :: Widget n
blockBullet = withAttr bulletAttr block_bullet


enemyAttr :: AttrName
enemyAttr = attrName "enemyAttr"

playerAttr :: AttrName
playerAttr  = attrName "playerAttr "

bulletAttr :: AttrName
bulletAttr  = attrName "bulletAttr"
