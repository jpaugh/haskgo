module Base where

import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)

import qualified Data.HashMap.Lazy as H

data Board =
        Board { layout :: Layout
              , size :: Size }
    deriving (Eq,Show)



data Player = Black
           | White
    deriving (Eq, Show)

type Piece = Player
type Point = (Int,Int)

data Size = Full
          | Mid
          | Small
    deriving (Eq, Show)

-- | Allows us to use interfaces while Board is still just a type synonym
newtype VisualBoard = VisualBoard [[Maybe Piece]]

instance Show VisualBoard where
        show (VisualBoard board) = showBoard (length board) board
          where
            -- checkColumnLengths size = all (\col -> length col == size) board
            showBoard size
                -- | checkColumnLengths size /= True = error "Invalid column length"
                | otherwise = intercalate "\n" . map showRow
            showRow :: [Maybe Piece] -> String
            showRow = foldl showCell ""

            showCell :: String -> Maybe Piece -> String
            showCell s Nothing = s ++ " ."
            showCell s (Just Black) = s ++ " B"
            showCell s (Just White) = s ++ " W"

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
    emptyCheck = H.member point $ layout board

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
