module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq)

type Board = ([Cell], Int)

type Position = (Int, Int)

instance Show Cell where
  show Empty     = "-"
  show (Taken X) = "X"
  show (Taken O) = "O"

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------
gameOver :: Board -> Bool 
gameOver b = any check lines
  where
    check (Empty : _) = False
    check ls = (null . tail . nub) ls
    lines = rows b ++ cols b ++ diags b
  
-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition s
  = readMaybe s' :: Maybe Position
  where
    s' = "(" ++ map (\c -> if c == ' ' then ',' else c) s ++ ")"

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove mark (x,y) (ps,size)
  | or [x < 0 , y < 0 , x >= size , y >= size] = Nothing -- invalid coords
  | (ps !! index) == Empty                     = Just newBoard
  | otherwise                                  = Nothing
  where
    index = x * size + y
    newBoard = (replace index (Taken mark) ps, size)

-------------------------------------------------------------------
-- I/O Functions

replacePrint :: String -> String
replacePrint [] = []
replacePrint (c:cs)
  | c == '[' || c == ']' = replacePrint cs
  | c == ','             = " " ++ replacePrint cs
  | otherwise            = c : replacePrint cs

prettyPrint :: Board -> IO ()
prettyPrint b
  = do
      let row =  intersperse "\n" $ map (replacePrint . show) (rows b)
      putStrLn (concat row)

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b p
  = do
      putStrLn "-------------------------------------"
      putStrLn "Please enter a position: "
      pos' <- getLine
      let pos'' = parsePosition pos'
      if isNothing pos''
        then do
          putStrLn "-------------------------------------"
          takeTurn b p
        else do
          let pos = fromJust pos''
          if isNothing $ tryMove p pos b
          then do
                  putStrLn "Invalid Coords"
                  putStrLn "-------------------------------------"
                  takeTurn b p
          else do
            let newBoard = fromJust (tryMove p pos b)
            return newBoard


-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b p
  = do
      prettyPrint b
      newBoard <- takeTurn b p
      if gameOver newBoard
        then do
                prettyPrint newBoard
                putStrLn $ "Congratulations, " ++ show p ++ " has won the game!"
        else do
          let p' = if p == X then O else X
          putStrLn "-------------------------------------"
          playGame newBoard p'



-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStrLn "Welcome to TicTacToe"
      putStrLn "Please enter the board size: "
      size' <- getLine
      let size  = (read size' :: Int)
          board@(b, s) = (replicate (size * size) Empty, size)
      playGame board X



-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3, testBoard4, testBoard5 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

testBoard4
  = ([Taken O, Taken O, Taken O,
      Empty, Taken X, Taken X,
      Taken X, Empty, Empty],
      3)

testBoard5
  = ([Taken O, Taken O, Empty,
      Taken X, Taken X, Taken X,
      Taken X, Taken O, Taken O],
      3)