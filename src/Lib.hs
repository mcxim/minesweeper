module Lib where

import qualified System.Random                 as R
import           Control.Monad                  ( (>=>) )
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Test.Hspec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = Flag | Closed | Open
  deriving Show

data Difficulty = Difficulty {height :: Int, width :: Int, numMines :: Int}
  deriving Show

beginner = Difficulty { height = 9, width = 9, numMines = 10 }
intermediate = Difficulty { height = 16, width = 16, numMines = 40 }
expert = Difficulty { height = 16, width = 30, numMines = 99 }

data Cell = Cell {number :: Int, state :: State}
  deriving Show

type Board = [[Cell]]

inRange :: (Num a, Ord a) => (a, a) -> a -> Bool
inRange (lower, upper) num = num <= upper && num >= lower

changeCell :: Board -> Cell -> (Int, Int) -> Board
changeCell board cell (row, col) =
  take row board
    ++ [take col (board !! row) ++ [cell] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

emptyBoard :: Difficulty -> Board
emptyBoard difficulty =
  replicate (height difficulty) $ replicate (width difficulty) (Cell 9 Closed)

composeMN :: (Monad m) => Int -> (a -> m a) -> a -> m a
composeMN 1 f = f
composeMN n f = f >=> composeMN (n - 1) f

addRandomMine :: Board -> IO Board
addRandomMine board = undefined

initBoard :: Difficulty -> IO Board
initBoard difficulty =
  let board = emptyBoard difficulty
      matrix =
          [ [ (row, col) | col <- [0 .. width difficulty] ]
          | row <- [0 .. height difficulty]
          ]
      idxs = concat matrix
      genMineIdxs =
          (\g -> map (idxs !!) $ take
              (numMines difficulty)
              (L.nub $ R.randomRs (0, length idxs - 1) g)
            )
            <$> R.newStdGen
  in  do
        mineIdxs <- genMineIdxs
        return
          . (map . map)
              (\idx ->
                if idx `elem` mineIdxs then Cell 0 Closed else Cell 9 Closed
              )
          $ matrix
