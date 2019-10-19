module Lib
  ( someFunc
  )
where

import           System.Random                  ( randomRIO )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = Flag | Closed | Open
  deriving Show

data Difficulty = Difficulty {height :: Int, width :: Int, numMines :: Int}
  deriving Show

beginner = Difficulty { height = 9, width = 9, numMines = 10 }
intermediate = Difficulty { height = 16, width = 16, numMines = 0 }
expert = Difficulty { height = 16, width = 30, numMines = 0 }

data Cell = Cell Int State
  deriving Show

type Board = [[Cell]]

inRange :: (Num a, Ord a) => (a, a) -> a -> Bool
inRange (lower, upper) num = num <= upper && num >= lower

emptyBoard :: Difficulty -> Board
emptyBoard difficulty =
  replicate (height difficulty) $ replicate (width difficulty) (Cell 9 Closed)

initBoard :: Difficulty -> IO Board
initBoard = undefined
