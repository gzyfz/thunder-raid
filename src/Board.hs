{-# LANGUAGE DeriveFunctor #-}
module Board 
  ( -- * Types
    Board
  , Piece (..)
  , Pos (..)

    -- * Board API
  , dimX
  , dimY
  , (!)
  , init
  , put
  , del
  , update
  , updateBullet
  , updateEnemy
  , refreshBullet
  , refreshEnemy
  , refreshAll
  , positions
  , emptyPositions

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 
import Data.Maybe (fromJust)

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos Piece

data Piece
  = Player
  | Enemy
  | Bullet
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe Piece
board ! pos = M.lookup pos board

dimX :: Int
dimY :: Int
dimX = 15
dimY = 10

positions :: [Pos]
positions = [ Pos r c | r <- [1..dimY], c <- [1..dimX] ] 

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = put (Just Player) (Pos 1 8) M.empty

put :: Maybe Piece -> Pos -> Board -> Board
put piece pos board
  | pRow pos > dimY || pRow pos < 1 = board
  | piece == Nothing                = board
  | otherwise                       = M.insert pos (fromJust piece) board

del :: Pos -> Board -> Board
del pos board = M.delete pos board

update :: Pos -> (Pos -> Pos) -> Board -> Board
update pos move board = put piece (move pos) (del pos board)
                          where piece = board ! pos

updateBullet :: Pos -> Board -> Board
updateBullet pos board
  | piece == Just Bullet  = update pos up board
  | otherwise             = board
  where piece = board ! pos

updateEnemy :: Board -> Pos -> Board
updateEnemy board pos
  | piece == Just Enemy   = update pos down board
  | otherwise             = board
  where piece = board ! pos

refreshBullet :: Board -> Board
refreshBullet board = foldr updateBullet board positions

refreshEnemy :: Board -> Board
refreshEnemy board = foldl updateEnemy board positions

refreshAll :: Board -> Board
refreshAll board = refreshEnemy (refreshBullet board)
-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos 
up p = p 
  { pRow = pRow p + 1 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = pRow p - 1 
  } 

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min dimX (pCol p + 1) 
  }
