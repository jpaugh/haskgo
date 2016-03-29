-- |Basic types and functions
module Base where

import Control.Arrow
import Control.Monad
import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate, intersperse, elemIndex)
import Data.Maybe
import Data.Unique
import System.Random
import qualified Data.HashMap.Lazy as H

-- | The game board is organized into a Cartesian plane with the origin
-- point at the center of the board. This arrangement makes it easier to
-- support multiple board sizes
data Board =
        Board { layout :: Layout
              , size :: Size }
    deriving (Eq)

-- | Board layout. A mapping to every stone on the board from its
-- @(x,y)@ coordinate.
type Layout = HashMap Point Stone

-- | Go has two opponents: white and black
data Player = Black
           | White
    deriving (Enum, Eq, Show)

-- | A synonym for 'Player', used to distinguish a game piece from one of
-- the opponents
type Stone = Player

-- | An @(x,y)@ coordinate
type Point = (Int,Int)

-- |A unique identifier for a collection of stones
type Group = Int

-- | A many-to-one mapping from every point on the board to the group that
-- contains it
type GroupPoints = HashMap Point Group
-- | A one-to-many mapping from every group on the board to each of the
-- points it contains
type Groups = HashMap Group [Point]

-- |Three board sizes are offered
data Size = Full  -- ^ 19x19 Standard/Competition
          | Mid   -- ^ 13x13
          | Small -- ^ 9x9
    deriving (Enum, Eq, Show)

-- | Graphical representation of a board, using Unicode characters
-- This is not meant for reading back in, although it is possible
instance Show Board where
        show (Board {..}) = format $ map showStoneAt $ enumeratePoints size
          where
            format :: String -> String
            format =
                let toLines = intercalate "\n"
                    spaceCols = map (intersperse '─')
                    separateRows = subgroup (sizeToInt size)
                    in toLines . spaceCols . separateRows

            showStoneAt :: Point -> Char
            showStoneAt = showStone . flip H.lookup layout

            showStone :: Maybe Stone -> Char
            showStone Nothing = '┼'
            showStone (Just Black) = '●'
            showStone (Just White) = '○'

enumeratePoints :: Size -> [Point]
-- | List of all points on the board. Increasing X with each col;
-- decreasing Y with each cell
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
-- and calculating the liveness of the opponent\'s adjacent groups
isPossibleMove point@(x,y) board = rangeCheck && emptyCheck
  where
    rangeCheck = let range = sizeToInt (size board) `quot` 2
                     in range >= abs x && range >= abs y
    emptyCheck = not $ H.member point $ layout board

placeStone :: Stone -> Point -> Board -> Board
-- |Place a stone on the board at the given point
placeStone stone point@(x,y) board
    | not $ isPossibleMove point board = board
    | otherwise = board {layout = layout board /$ H.insert point stone}

sizeToInt :: Size -> Int
-- | The size of the board as an 'Int'
sizeToInt Full  = 19
sizeToInt Mid   = 13
sizeToInt Small =  9

--
-- Utility functions
--

-- | @($)@ with its arguments reversed
(/$) :: a -> (a -> b) -> b
(/$) = flip ($)
infixr 0 /$

subgroup :: Int -> [a] -> [[a]]
-- | Divide a list\'s elements into equally sized groups of sublists
subgroup _ [] = []
subgroup n xs
  | n < 1 = error "Negative n"
  | otherwise = let (l, rest) = splitAt n xs
                    in l : subgroup n rest
