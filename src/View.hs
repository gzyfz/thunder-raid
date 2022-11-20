{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile (reverseList [ mkRow s row | row <- [1..dim] ])
       where       
      reverseList [] = []
      reverseList (x:xs) = reverseList xs ++ [x]



      

header :: PlayState -> String
header s = printf "Thunder raid , row = %d, col = %d"  (pRow p) (pCol p)
  where 
    p    = psPos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..dim] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurr s r c = center (mkXO (Just O ))
  | psTurn s == O&& r==dim&& c==1 = center (mkXO (Just X )) 
  | otherwise    = center (mkXO Nothing )



-- mkCell' :: PlayState -> Int -> Int -> Widget n
-- -- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
-- mkCell' s r c = center (mkXO xoMb)
--   where 
--     xoMb 
--       | r == 1 && c==1  = Just X 
--       | r == dim && c==1    = Just O 
--       | otherwise = Nothing

  

mkXO :: Maybe XO -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO


blockB, blockX, blockO :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockX = vBox [ str "   _   ",
                str "<  |  >",
                str "  | |  ",
                str "   V   " ]
blockO = vBox [ str "    *    ",
                str "   |||   ",
                str "  * | *  ",
                str "<| | | |>",
                str " _* | *_ "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) =  vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget
