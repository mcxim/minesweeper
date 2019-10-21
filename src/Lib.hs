module Lib where

import qualified System.Random                 as R
import           Control.Monad                  ( (>=>) )
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Test.Hspec
import           Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = Flag | Closed | Open
  deriving (Show, Eq)

data Move = FlagIt | OpenIt
  deriving (Eq, Show)

data Difficulty = Difficulty {height :: Int, width :: Int, numMines :: Int}
  deriving (Show, Eq)

beginner = Difficulty { height = 9, width = 9, numMines = 10 }
intermediate = Difficulty { height = 16, width = 16, numMines = 40 }
expert = Difficulty { height = 16, width = 30, numMines = 99 }

data Cell = Cell {number :: Int, state :: State}
  deriving (Show, Eq)

closedMine = Cell 9 Closed
closedEmpty = Cell 0 Closed

closedPeaceful :: Cell -> Bool
closedPeaceful (Cell number state) = state == Closed && number /= 9

type Board = [[Cell]]

showCell (Cell _      Closed) = "%"         -- Closed cell
showCell (Cell _      Flag  ) = "P"         -- Flag
showCell (Cell 9      Open  ) = "*"         -- Mine
showCell (Cell 0      Open  ) = "_"         -- No number
showCell (Cell number Open  ) = show number -- Number

prettyRepr :: Board -> String
prettyRepr = unlines . map (unwords . map showCell)

prettyPrint :: Board -> IO ()
prettyPrint = putStrLn . prettyRepr

inRange :: (Num a, Ord a) => a -> (a, a) -> Bool
inRange num (lower, upper) = num <= upper && num >= lower

opOnCell :: (Cell -> Cell) -> Board -> (Int, Int) -> Board
opOnCell f board (row, col) =
  take row board
    ++ [take col (board !! row) ++ [f cell] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board
  where cell = board !! row !! col

emptyBoard :: Difficulty -> Board
emptyBoard difficulty =
  replicate (height difficulty) $ replicate (width difficulty) (Cell 9 Closed)

composeMN :: (Monad m) => Int -> (a -> m a) -> a -> m a
composeMN 1 f = f
composeMN n f = f >=> composeMN (n - 1) f

addRandomMine :: Board -> IO Board
addRandomMine board = undefined

neighborIdxs :: [[a]] -> (Int, Int) -> [(Int, Int)]
neighborIdxs board (row, col) =
  [ (row', col')
  | row' <- [row - 1 .. row + 1]
  , row' `inRange` (0, length board - 1)
  , col' <- [col - 1 .. col + 1]
  , col' `inRange` (0, length (head board) - 1)
  , not (row' == row && col' == col)
  ]

neighbors :: Board -> (Int, Int) -> [Cell]
neighbors board (row, col) =
  map (\(row', col') -> board !! row' !! col') (neighborIdxs board (row, col))

-- | Returns a new board given a difficulty.
-- Testing: initBoard intermediate >>= (prettyPrint . unlockBoard)
initBoard :: Difficulty -> [(Int, Int)] -> IO Board
initBoard difficulty safeSpots =
  let matrix =
          [ [ (row, col) | col <- [0 .. width difficulty - 1] ]
          | row <- [0 .. height difficulty - 1]
          ]
      safeExpanded = L.nub $ do
        spot <- safeSpots
        let spotNeighbors = neighborIdxs matrix spot
        spot : spotNeighbors
      idxs = filter (`notElem` safeExpanded) (concat matrix)
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
        return
          [ [ if bare !! row !! col == closedMine
                then closedMine
                else Cell numSurMines Closed
            | col <- [0 .. width difficulty - 1]
            , let numSurMines = length
                    (filter (== closedMine) (neighbors bare (row, col)))
            ]
          | row <- [0 .. height difficulty - 1]
          ]

unlockCell :: Cell -> Cell
unlockCell (Cell number _) = Cell number Open

flagCell :: Cell -> Cell
flagCell (Cell number _) = Cell number Flag

unlockBoard :: Board -> Board
unlockBoard = (map . map) unlockCell

unlockEmptyFrom :: Board -> (Int, Int) -> Board
unlockEmptyFrom board coords@(row, col)
  | state cell == Open = board
  | number cell `inRange` (1, 8) = opOnCell unlockCell board coords
  | number cell == 0 = L.foldl' unlockEmptyFrom
                                (opOnCell unlockCell board coords)
                                (neighborIdxs board coords)
  | otherwise = undefined
  where cell = board !! row !! col

doMove :: Board -> (Int, Int) -> Move -> Either Board Board
doMove board coords@(row, col) move
  | move == FlagIt       = Right (opOnCell flagCell board coords)
  | cell == closedMine   = Left (unlockBoard board)
  | state cell == Closed = Right (unlockEmptyFrom board coords)
  | otherwise            = Right board
  where cell = board !! row !! col

isGameWon :: Board -> Bool
isGameWon = not . any (any closedPeaceful)

runGame :: IO ()
runGame = do
  putStrLn
    "Please choose difficulty level:\nb - beginner,\ni - intermediate,\ne - expert"
  difficultyInput <- getLine
  let difficulty = case head difficultyInput of
        'b' -> beginner
        'i' -> intermediate
        'e' -> expert
        _   -> beginner
  case difficulty of
    beginner     -> putStrLn "The difficulty is set to beginner."
    intermediate -> putStrLn "The difficulty is set to intermediate."
    expert       -> putStrLn "The difficulty is set to expert."
  putStrLn
    "Input first move (safe). Format: <row letter (a,b..)><col number (1,2..)>"
  firstMove <- getLine
  let row = ord (head firstMove) - 97
  let col = read (tail firstMove) :: Int
  board <- initBoard difficulty [(row, col)]
  putStrLn "dummy"

-- gameLoop :: board -> IO ()
-- gameLoop board = do
--   board <- initBoard difficulty [firstMove]
--   putStrLn "dummy"
