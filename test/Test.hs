import Test.Tasty
import Common
import Prelude hiding (maximum)
import qualified Board as B
import qualified Control as C
import qualified Data.Map as M 

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
initCrashBoard = M.insert (B.Pos 4 8) B.Enemy  (M.insert (B.Pos 3 8) B.Bullet B.init)

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
    ,scoreTest ((\_ -> B.refreshEnemy initPlayerCrash), (), resultPlayerCrash, 1, "Player Crash test")
    ,scoreTest ((\_ -> B.refreshExplosion resultCrashBoard), (), resultExplosion, 1, "Player Crash test")
    ]
    where 
      scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
      scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)



