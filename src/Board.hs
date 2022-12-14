
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
  , sumExplosion
  , getPlayerPos
  , refreshExplosion

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
  | Explosion
  deriving (Eq, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

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
  | original /= Nothing             = M.insert pos (Explosion) board
  | otherwise                       = M.insert pos (fromJust piece) board
  where original = board ! pos

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

updateExplosion :: Pos -> Board -> Board
updateExplosion pos board
  | piece == Just Explosion = del pos board
  | otherwise               = board
  where piece = board ! pos

countExplosion :: Board -> Pos -> Int -> Int
countExplosion board pos n
  | piece == Just Explosion = n + 1
  | otherwise               = n
  where piece = board ! pos

sumExplosion :: Board -> Int
sumExplosion board = foldr (countExplosion board) 0 positions

scanPlayerPos :: Board -> Pos -> Pos -> Pos
scanPlayerPos board newpos oldpos
  | piece == Just Player = newpos
  | otherwise            = oldpos
  where piece = board ! newpos

getPlayerPos :: Board -> Pos
getPlayerPos board = foldr (scanPlayerPos board) (Pos 0 0) positions

refreshBullet :: Board -> Board
refreshBullet board = foldr updateBullet board positions

refreshEnemy :: Board -> Board
refreshEnemy board = foldl updateEnemy board positions

refreshExplosion :: Board -> Board
refreshExplosion board = foldr updateExplosion board positions

refreshAll :: Board -> Board
refreshAll board = refreshEnemy (refreshBullet (refreshExplosion board))

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
