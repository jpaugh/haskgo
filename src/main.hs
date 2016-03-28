module Main where

import Base
import Group
import Liveness
import Random

import Control.Arrow
import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate, intersperse, elemIndex)
import Data.Maybe
import System.Random
import qualified Data.HashMap.Lazy as H

{-
-
- To calculate groups, we need to convert the board to two lists of points:
- one for each player's pieces.^1
-
- Then, we can use this mapping to carve up the board into groups
-
- 1: This format might actually be preferable to the display representation
- currently used.
-
- (0,0) is the center pice. This means we can use the same `empty` function
- for all board types
-}

type Groups = [[Point]]
type PlayerPoints = [Point]

{-

splitBoard :: Board -> (PlayerPoints,PlayerPoints)
-- | Split the board into two; one contains only White's pieces, while the
-- other contains only Black's
splitBoard = foldl onRow
  where
    onRow :: (PlayerPoints, PlayerPoints) -> (Int,[Piece]) -> Use a HashMap?
-}


main = putStrLn "Hello, World!"
