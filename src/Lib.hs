module Lib where

import qualified System.Random                 as R
import           Control.Monad                  ( (>=>) )
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Test.Hspec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = Flag | Closed | Open
  deriving (Show, Eq)

data Difficulty = Difficulty {height :: Int, width :: Int, numMines :: Int}
  deriving (Show, Eq)

beginner = Difficulty { height = 9, width = 9, numMines = 10 }
intermediate = Difficulty { height = 16, width = 16, numMines = 40 }
expert = Difficulty { height = 16, width = 30, numMines = 99 }

data Cell = Cell {number :: Int, state :: State}
  deriving (Show, Eq)

closedMine = Cell 9 Closed
closedEmpty = Cell 0 Closed

type Board = [[Cell]]

inRange :: (Num a, Ord a) => a -> (a, a) -> Bool
inRange num (lower, upper) = num <= upper && num >= lower

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

neighbors :: Board -> (Int, Int) -> [Cell]
neighbors board (row, col) =
  [ board !! row' !! col'
  | row' <- [row - 1 .. row + 1]
  , row' `inRange` (0, length board - 1)
  , col' <- [col - 1 .. col + 1]
  , col' `inRange` (0, length (head board) - 1)
  , not (row' == 0 && col' == 0)
  ]

-- | Returns a new board given a difficulty.
-- Testing: initBoard intermediate >>= (prettyPrint . unlockBoard)
initBoard :: Difficulty -> IO Board
initBoard difficulty =
  let matrix =
          [ [ (row, col) | col <- [0 .. width difficulty - 1] ]
          | row <- [0 .. height difficulty - 1]
          ]
      idxs = concat matrix
      genMineIdxs =
          (\g -> map (idxs !!) $ take
              (numMines difficulty)
              (L.nub $ R.randomRs (0, length idxs - 1) g)
            )
            <$> R.newStdGen
      genBare = do
        mineIdxs <- genMineIdxs
        return $ (map . map)
          (\idx -> if idx `elem` mineIdxs then closedMine else closedEmpty)
          matrix
  in  do
        bare <- genBare
        return $ do -- List monad
          row <- [0 .. height difficulty - 1]
          return $ do -- List monad
            col <- [0 .. width difficulty - 1]
            let numSurMines =
                  length (filter (== closedMine) (neighbors bare (row, col)))
            return $ if bare !! row !! col == closedMine
              then closedMine
              else Cell numSurMines Closed

unlockCell :: Cell -> Cell
unlockCell (Cell number _) = Cell number Open

unlockBoard :: Board -> Board
unlockBoard = (map . map) unlockCell

showCell :: Cell -> String
showCell (Cell _      Closed) = "%" -- Closed cell
showCell (Cell _      Flag  ) = "P" -- Flag
showCell (Cell 9      Open  ) = "*" -- Mine
showCell (Cell 0      Open  ) = "_" -- No number
showCell (Cell number Open  ) = show number -- Number

prettyRepr :: Board -> String
prettyRepr = unlines . map (unwords . map showCell)

prettyPrint :: Board -> IO ()
prettyPrint = putStrLn . prettyRepr


