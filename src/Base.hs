module Base where

import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate, intersperse)
import Data.Maybe (isJust, isNothing)

import qualified Data.HashMap.Lazy as H

data Board =
        Board { layout :: Layout
              , size :: Size }
    deriving (Eq)

data Player = Black
           | White
    deriving (Eq, Show)

type Piece = Player
type Point = (Int,Int)

data Size = Full
          | Mid
          | Small
    deriving (Eq, Show)

instance Show Board where
        show (Board {..}) = format $ map showPieceAt $ enumeratePoints size
          where
            format :: String -> String
            format =
                let toLines = intercalate "\n"
                    spaceCols = map (intersperse '-')
                    separateRows = subgroup (sizeToInt size)
                    in toLines . spaceCols . separateRows

            showPieceAt :: Point -> Char
            showPieceAt = showPiece . flip H.lookup layout

            showPiece :: Maybe Piece -> Char
            showPiece Nothing = '+'
            showPiece (Just Black) = 'B'
            showPiece (Just White) = 'W'

enumeratePoints :: Size -> [Point]
-- | Increasing X with each col; decreasing Y with each cell
enumeratePoints size = [(x,y) | y <- ys, x <- xs]
  where
    max = sizeToInt size `quot` 2
    min = -max
    xs = [min..max]
    ys = [max, max-1 .. min]


empty :: Size -> Board
-- | An empty board, ready for play
empty size = Board { layout = H.empty, size = size }

isPossibleMove :: Point -> Board -> Bool
-- | Check whether a move is possible at the given board position; it does
-- not calculate liveness (alive or dead?), since that involves grouping,
-- and calculating the liveness of the opponent's adjacent groups
isPossibleMove point@(x,y) board = rangeCheck && emptyCheck
  where
    rangeCheck = let range = sizeToInt (size board) `quot` 2
                     in range >= abs x && range >= abs y
    emptyCheck = not $ H.member point $ layout board

placePiece :: Piece -> Point -> Board -> Board
placePiece piece point@(x,y) board
    | not $ isPossibleMove point board = board
    | otherwise = board {layout = layout board /$ H.insert point piece}

sizeToInt :: Size -> Int
-- | The size of the board as an `Int`
sizeToInt Full  = 19
sizeToInt Mid   = 13
sizeToInt Small =  9


(/$) :: a -> (a -> b) -> b
(/$) = flip ($)
infixr 0 /$

subgroup :: Int -> [a] -> [[a]]
-- | Divide a list's elements into equally sized groups of sublists
subgroup _ [] = []
subgroup n xs
  | n < 1 = error "Negative n"
  | otherwise = let (l, rest) = splitAt n xs
                    in l : subgroup n rest
