module Main where

import Data.Maybe (isJust, isNothing)
import Data.List (intercalate)

data Player = Black
           | White
    deriving (Eq, Show)

data Size = Full
          | Mid
          | Small
    deriving (Eq, Show)

-- TODO: Newtype ADTs
type Piece = Maybe Player
type Point = (Int,Int)
type Board = [[Piece]]

-- | Allows us to use interfaces while Board is still just a type synonym
newtype BoardADT = BoardADT Board

instance Show BoardADT where
        show (BoardADT board) = showBoard (length board) board
          where
            -- checkColumnLengths size = all (\col -> length col == size) board
            showBoard size
                -- | checkColumnLengths size /= True = error "Invalid column length"
                | otherwise = intercalate "\n" . map showRow
            showRow :: [Piece] -> String
            showRow = foldl showCell ""

            showCell :: String -> Piece -> String
            showCell s Nothing = s ++ " ."
            showCell s (Just Black) = s ++ " B"
            showCell s (Just White) = s ++ " W"

empty :: Size -> Board
-- | An empty board, ready for play
empty size = replicate (num size) $
                replicate (num size) Nothing
  where
    num Full = 19
    num Mid = 13
    num Small = 9

(/$) :: a -> (a -> b) -> b
(/$) = flip ($)
infixr 0 /$

isPossibleMove :: Point -> Board -> Bool
-- | Check whether a move is possible at the given board position; it does
-- not calculate liveness (alive or dead?), since that involves grouping,
-- and calculating the liveness of the opponent's adjacent groups
isPossibleMove (x,y) board = rangeCheck && emptyCheck
  where
    rangeCheck = length board > x && length (board !! x) > y
    emptyCheck = isNothing $ ((board !! x) !! y)

placePiece :: Player -> Point -> Board -> Board
placePiece player point@(x,y) board
    | not $ isPossibleMove point board = board
    | otherwise = board /$ modifyNth x updateCell
        where
            updateCell :: [Piece] -> [Piece]
            updateCell = replaceNth y (Just player)

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth n f [] = []
modifyNth n f (x:xs)
    | n == 0 = f x : xs
    | otherwise = x : modifyNth (n - 1) f xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal = modifyNth n $ const newVal


main = putStrLn "Hello, World!"
