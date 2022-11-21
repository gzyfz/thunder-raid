module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Board
import Graphics.Vty()

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

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
mkPiece (Just Player) = block_player
mkPiece (Just Enemy) = block_enemy
mkPiece (Just Bullet) = block_bullet

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

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : bs)
hTile _      = emptyWidget
