module Tests (Tests.main) where
import IC.TestSuite
import TicTacToe hiding (main)


gameOverTestCases
   = [ testBoard1 ==> True
     , testBoard2 ==> False
     , testBoard3 ==> True
     , testBoard4 ==> True
     , testBoard5 ==> True
     , testBoard6 ==> False
     ]

parsePositionTestCases
   = [
       ("0 2")   ==> (Just (0,2))
     , ("0 -8")  ==> (Just (0,-8))
     , ("-4 1")  ==> (Just (-4,1))
     , ("0 %1")  ==> (Nothing)
     , ("")      ==> (Nothing)
     , ("1 2 3") ==> (Nothing)
     , ("a b")   ==> (Nothing)
     , ("2 3")   ==> (Just (2,3))
     , ("2 3 ")  ==> (Nothing)
     , (" ")     ==> (Nothing)
     ]

tryMoveTestCases
  = [
      (X,(0,0),testBoard2)  ==> (Nothing)
    , (O,(-1,2),testBoard2) ==> (Nothing)
    , (O,(0,-1),testBoard2) ==> (Nothing)
    , (O,(1,1),testBoard2)  ==> (Just ([Taken X,Empty,Empty,Taken O],2))
    , (O,(3,3),testBoard1)  ==> (Just ([Taken O,Taken X,Empty,Taken O,Taken O,
                                Empty,Taken X,Taken X,Taken O,Empty,Empty,Taken
                                X,Taken O,Taken X,Empty,Taken O],4))
    , (X,(2,2),testBoard4)  ==> (Just ([Taken O, Taken O, Taken O,Empty,Taken X,Taken X, Taken X,Empty, Taken X],3))
    , (X,(1,2),testBoard5)  ==> (Nothing)
    , (X,(0,2),testBoard5)  ==> (Just ([Taken O,Taken O,Taken X,Taken X,Taken X,Taken X,Taken X,Taken O,Taken O],3))
    , (O,(0,3),testBoard3)  ==> (Nothing)
    , (O,(0,2),testBoard3)  ==> Just ([Taken O,Taken X,Taken O,Taken O,Taken X,Taken O,Empty,Taken X,Taken X,Empty,Empty,Empty,Taken X,Taken O,Taken O,Taken O,Taken X,Empty,Empty,Taken X,Taken X,Empty,Taken O,Empty,Empty],5)
    ]

-- You can add your own test cases above

allTestCases
  = [
      TestCase "gameOver" (gameOver)
               gameOverTestCases
    , TestCase "parsePosition" (parsePosition)
               parsePositionTestCases
    , TestCase "tryMove" (uncurry3 tryMove)
               tryMoveTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests
