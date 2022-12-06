import Test.Tasty
import Common
import Prelude hiding (maximum)
import qualified Board as B
import qualified Control as C
import qualified Data.Map as M
import qualified Model 
import qualified Model
import qualified Model

main :: IO ()
main = runTests
  [ probBoard
  , probControl
  ]

initTestBoard:: B.Board
initTestBoard = M.insert (B.Pos 2 8) B.Bullet (M.insert (B.Pos 10 8) B.Enemy  B.init)

resultEnemyMove:: B.Board
resultEnemyMove = M.insert (B.Pos 2 8) B.Bullet (M.insert (B.Pos 9 8) B.Enemy  B.init)

resultBulletMove:: B.Board
resultBulletMove = M.insert (B.Pos 3 8) B.Bullet (M.insert (B.Pos 10 8) B.Enemy  B.init)

initCrashBoard:: B.Board
initCrashBoard = M.insert (B.Pos 4 8) B.Enemy (M.insert (B.Pos 3 8) B.Bullet B.init)

resultCrashBoard:: B.Board
resultCrashBoard = M.insert (B.Pos 4 8) B.Explosion  B.init

initEdgeBoard:: B.Board
initEdgeBoard = M.insert (B.Pos 1 7) B.Enemy  B.init

resultEdgeMove:: B.Board
resultEdgeMove = B.init

initPlayerCrash:: B.Board
initPlayerCrash = M.insert (B.Pos 2 8) B.Enemy  B.init

resultPlayerCrash:: B.Board
resultPlayerCrash = M.insert (B.Pos 1 8) B.Explosion M.empty

resultExplosion:: B.Board
resultExplosion = B.init


probBoard:: Score -> TestTree
probBoard sc = testGroup "Board"
    [scoreTest ((\_ -> B.refreshEnemy initTestBoard), (), resultEnemyMove, 1, "Enemy move test")
    ,scoreTest ((\_ -> B.refreshBullet initTestBoard), (), resultBulletMove, 1, "Bullet move test")
    ,scoreTest ((\_ -> B.refreshBullet initCrashBoard), (), resultCrashBoard, 1, "Crash bullet test")
    ,scoreTest ((\_ -> B.refreshEnemy initEdgeBoard), (), resultEdgeMove, 1, "Enemy Edge test")
    ,scoreTest (\_ -> B.refreshEnemy initPlayerCrash, (), resultPlayerCrash, 1, "Player Crash test")
    ,scoreTest (\_ -> B.refreshExplosion resultCrashBoard, (), resultExplosion, 1, "Player Crash test")
    ,scoreTest (\_ -> B.getPlayerPos initPlayerCrash, (), B.Pos 1 8, 1, "Player exist")
    ,scoreTest (\_ -> B.getPlayerPos resultPlayerCrash, (), B.Pos 0 0, 1, "Player doesn't exist")
    ]
    where
      scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
      scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)


resultInitPS:: Model.PlayState
resultInitPS = Model.PS
  { Model.psBoard  = M.insert (B.Pos 9 8) B.Enemy  B.init,
    Model.playerPos = B.Pos 1 8,
    Model.playerScore  = 0,
    Model.playerTime  = 1
  }

resultBulletPS:: Model.PlayState
resultBulletPS = Model.PS
  {
    Model.psBoard  = M.insert (B.Pos 2 8) B.Bullet  B.init,
    Model.playerPos = B.Pos 1 8,
    Model.playerScore  = 0,
    Model.playerTime  = 0
  }

initScorePS::Model.PlayState
initScorePS = Model.PS
  {
    Model.psBoard  = initCrashBoard,
    Model.playerPos = B.Pos 1 8,
    Model.playerScore  = 0,
    Model.playerTime  = 0
  }

resultScorePS::Model.PlayState
resultScorePS = Model.PS
  {
    Model.psBoard  = resultCrashBoard,
    Model.playerPos = B.Pos 1 8,
    Model.playerScore  = 1,
    Model.playerTime  = 1
  }

-- initGameFinishPS:: PlayState

probControl:: Score -> TestTree
probControl sc = testGroup "Control"
  [
    scoreTest (\_ -> C.updateAllAndAddEnemy Model.init 8, (), resultInitPS, 1, "update playState")
  , scoreTest (\_ -> C.generate B.Bullet 8  Model.init, (), resultBulletPS, 1, "update position of bullet")
  , scoreTest (\_ -> C.updateAllOnly initScorePS, (), resultScorePS, 1, "get score")
  ]
  where 
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)