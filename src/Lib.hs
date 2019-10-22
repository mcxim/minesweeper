module Lib where

import qualified System.Random                 as R
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Test.Hspec
import           Data.Char
import           Safe                           ( headMay
                                                , tailMay
                                                )
import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import           System.Exit                    ( exitSuccess )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data State = Flag | Closed | Open
  deriving (Show, Eq)

data Move = FlagIt | OpenIt | UnFlagIt
  deriving (Eq, Show)

data Difficulty = Difficulty {height :: Int, width :: Int, numMines :: Int}
  deriving (Show, Eq)

data Cell = Cell {number :: Int, state :: State}
  deriving (Show, Eq)

type Board = [[Cell]]

beginner = Difficulty { height = 9, width = 9, numMines = 10 }
intermediate = Difficulty { height = 16, width = 16, numMines = 40 }
expert = Difficulty { height = 16, width = 30, numMines = 99 }

closedMine = Cell 9 Closed
closedEmpty = Cell 0 Closed

dummyBoard :: Difficulty -> Board
dummyBoard difficulty =
  replicate (height difficulty) $ replicate (width difficulty) (Cell 9 Closed)

showCell :: Cell -> String
showCell (Cell _      Closed) = "%"         -- Closed cell
showCell (Cell _      Flag  ) = "P"         -- Flag
showCell (Cell 9      Open  ) = "*"         -- Mine
showCell (Cell 0      Open  ) = "-"         -- No number
showCell (Cell number Open  ) = show number -- Number

prettyRepr :: Board -> String
prettyRepr board =
  "\n    "
    ++ unwords (map (show . (`mod` 10)) [1 .. (length $ head board)])
    ++ "\n\n"
    ++ ( unlines
       . zipWith (\c rest -> c : "   " ++ rest) ['a' ..]
       . map (unwords . map showCell)
       $ board
       )

prettyPrint :: Board -> IO ()
prettyPrint = putStrLn . prettyRepr

inRange :: (Num a, Ord a) => a -> (a, a) -> Bool
inRange num (lower, upper) = num <= upper && num >= lower

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

opOnCell :: (Cell -> Cell) -> Board -> (Int, Int) -> Board
opOnCell f board (row, col) =
  take row board
    ++ [take col (board !! row) ++ [f cell] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board
  where cell = board !! row !! col

unlockCell :: Cell -> Cell
unlockCell (Cell number _) = Cell number Open

flagCell :: Cell -> Cell
flagCell (Cell number _) = Cell number Flag

unFlagCell :: Cell -> Cell
unFlagCell (Cell number _) = Cell number Closed

unlockBoard :: Board -> Board
unlockBoard = (map . map) unlockCell

unlockEmptyFrom :: Board -> (Int, Int) -> Board
unlockEmptyFrom board coords@(row, col)
  | state cell == Open = board
  | number cell `inRange` (1, 8) = opOnCell unlockCell board coords
  | otherwise = L.foldl' unlockEmptyFrom
                         (opOnCell unlockCell board coords)
                         (neighborIdxs board coords)
  where cell = board !! row !! col

doMove :: Board -> Maybe (Move, (Int, Int)) -> (Board, Bool, String)
doMove board Nothing = (board, True, "Invalid input.")
doMove board (Just (move, coords))
  | state cell == Open
  = (board, True, "This cell is already open.")
  | move == FlagIt
  = (opOnCell flagCell board coords, True, "Cell successfuly flagged.")
  | move == UnFlagIt
  = (opOnCell unFlagCell board coords, True, "Cell successfuly unflagged.")
  | state cell == Flag
  = (board, True, "You need to unflag this cell before you can open it.")
  | number cell == 9
  = (unlockBoard board, False, "Whoops, you stepped on a mine!")
  | otherwise
  = (unlockEmptyFrom board coords, True, "That was an empty cell.")
  where cell = board !! fst coords !! snd coords

gameWon :: Board -> Bool
gameWon = not . any (any closedPeaceful)
  where closedPeaceful (Cell number state) = state == Closed && number /= 9

inputMove :: Difficulty -> IO (Maybe (Move, (Int, Int)))
inputMove difficulty = do
  putStrLn
    "Input next move. Format: <o for open, f for flag, u for unflag><row letter (a,b..)><col number (1,2..)"
  input <- getLine
  if input == "q"
    then exitSuccess
    else
      let move = evalInput difficulty input
      in  if isNothing move
            then putStrLn "Invalid input." >> inputMove difficulty
            else return move

inputFirstMove :: Difficulty -> IO (Maybe (Move, (Int, Int)))
inputFirstMove difficulty = do
  prettyPrint $ dummyBoard difficulty
  putStrLn
    "Input first move (safe). Format: <row letter (a,b..)><col number (1,2..)>"
  input <- getLine
  if input == "q"
    then exitSuccess
    else
      let move = evalInput difficulty ("o" ++ input)
      in  if isNothing move
            then putStrLn "Invalid input." >> inputFirstMove difficulty
            else return move

evalInput :: Difficulty -> String -> Maybe (Move, (Int, Int))
evalInput difficulty input = do
  move'' <- headMay input
  let move' | move'' == 'o' = Just OpenIt
            | move'' == 'f' = Just FlagIt
            | move'' == 'u' = Just UnFlagIt
            | otherwise     = Nothing
  move <- move'
  t    <- tailMay input
  row' <- headMay t
  let row = ord row' - 97
  t'   <- tailMay t
  col' <- readMaybe t' :: Maybe Int
  let col = col' - 1
  if row
       `inRange` (0, height difficulty - 1)
       &&        col
       `inRange` (0, width difficulty - 1)
    then return (move, (row, col))
    else Nothing

inputDifficulty :: IO Difficulty
inputDifficulty = do
  putStrLn
    "Please choose difficulty level:\nb - beginner,\ni - intermediate,\ne - expert"
  difficultyInput <- getLine
  case head difficultyInput of
    'b' -> putStrLn "Difficulty set to beginner." >> return beginner
    'i' -> putStrLn "Difficulty set to intermediate." >> return intermediate
    'e' -> putStrLn "Difficulty set to expert." >> return expert
    'q' -> exitSuccess
    _   -> putStrLn "Invalid input." >> inputDifficulty

gameLoop :: Difficulty -> Board -> Maybe (Move, (Int, Int)) -> IO ()
gameLoop difficulty board maybeMoveCoords
  | gameWon board = putStrLn "You won!"
  | otherwise = do
    let (newBoard, continue, message) = doMove board maybeMoveCoords
    prettyPrint newBoard
    putStrLn message
    if continue
      then do
        newMove <- inputMove difficulty
        gameLoop difficulty newBoard newMove
      else putStrLn "Game over."

runGame :: IO ()
runGame = do
  difficulty <- inputDifficulty
  maybeMove  <- inputFirstMove difficulty
  let (_, (row, col)) = fromMaybe undefined maybeMove
  board <- initBoard difficulty [(row, col)]
  gameLoop difficulty board maybeMove
