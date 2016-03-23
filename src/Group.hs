-- | Functions for working with groups
--
-- In game, a group is a collection of stones of the same color which are
-- adjacent, where adjacent means touching horizontally or vertically (but
-- not diagonally).
--
-- Algorithmically, a group is represented by a collection of points
-- associated with a unique integer. Each point identifies a position on
-- the board which is occupied by a stone of either color.
--
-- The groups are identified by one of one of two inter-convertible structures:
--
-- * `type Groups = HashMap Group [Point]` is the obvious representation,
-- with each group referring to its list of points
-- * `type GroupPoints = HashMap Point Group` maps each point to the group
-- which contains it; multiple keys map to the same entry. This
-- representation is easier to construct from a given board position
--
-- The groups data structures (`Groups`, `GroupPoints`) do not retain
-- player information; but one may infer that adjacent groups are of
-- different colors, and the color can be retrieved by consulting the board
-- itself
module Group where

import Base

import Control.Monad
import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate, elemIndex, sort, groupBy)
import Data.Maybe
import qualified Data.HashMap.Lazy as H

groupBoard :: Board -> Groups
-- | Collect all of the points on the board into their respective groups
groupBoard board@(Board {..}) = foldl classify H.empty $ zip points groupNumbers
  where
    classify groups (point,nextGroup) = res
      where
        res = if H.member point groups then groups else go
        go = classifyPoint point nextGroup board groups
    points = enumeratePoints size
    groupNumbers = [1..81]

classifyPoint :: Point -> Group -> Board -> Groups -> Groups
classifyPoint point nextGroup board@(Board {..}) groups =
        case lookupBoard point of
            Nothing -> groups
            Just piece -> findGroupFor piece
  where

    -- Either find an existing, adjacent group to add this piece to, or
    -- start a new one
    findGroupFor :: Piece -> Groups
    findGroupFor piece = insertAndFindMembers $
        case adjGroupIdx of
            Nothing -> nextGroup
            Just i -> extractGroup i
      where
        adjGroupIdx = elemIndex (Just piece) adjPieces
        insertAndFindMembers group =
            membership piece point board group $
                H.insert point group groups

    extractGroup :: Int -> Group
    extractGroup i = fromMaybe nextGroup $ H.lookup (adjPoints !! i) groups
        -- NB: We have already proven that the group at (adjPoints !! i)
        -- exists; but, we fail to express that in a "safe" way. However,
        -- if nextGroup is actually used here, then we have made an error


    -- We use -PointsBefore, because the "after" points will be calculated
    -- later; no need to do double work
    -- NB: adjPoints and adjPieces are parallel; they must have the same
    -- ordering and length
    adjPoints = adjacentPointsBefore size point
    adjPieces = map lookupBoard adjPoints

    lookupBoard :: Point -> Maybe Piece
    lookupBoard = flip H.lookup layout

membership :: Piece -> Point -> Board -> Group -> Groups -> Groups
-- | Search for group members at neighbors of the given point; if any
-- members are found, add them to the group, and continue by searching
-- their neighbors.
--
-- This function is needed because we cannot reliably coalesce all group
-- members with only a single pass in an in-order traversal; thus
-- `membership` traverses ahead of the normal pass
membership player point board@(Board {..}) group groups = go groups
  where
    go = flip (foldl searchMember) (adjacentPoints size point)

    searchMember groups point
        | H.member point groups = groups            -- Already found
        | H.lookup point layout /= Just player = groups -- No piece or wrong player
        | otherwise = membership player point board group $ H.insert point group groups
          where

adjacentPoints :: Size -> Point -> [Point]
-- | All of the points which are adjacent to the given point; the size
-- tells us where the edge of the board is, so we don't count non-existent
-- points
adjacentPoints size point@(x,y) = adjacentHelper size point points
  where
    points = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

adjacentPointsBefore :: Size -> Point -> [Point]
-- | The adjacent points which come before the given point in the
-- `enumeratePoints`
adjacentPointsBefore size point@(x,y) = adjacentHelper size point points
  where
    points = [(x-1,y), (x,y+1)]

adjacentPointsAfter :: Size -> Point -> [Point]
-- | The adjacent points which come after the given point in the
-- `enumeratePoints`
adjacentPointsAfter size point@(x,y) = adjacentHelper size point points
  where
    points = [(x+1,y), (x,y-1)]

adjacentHelper :: Size -> Point -> [Point] -> [Point]
adjacentHelper size (x,y) = filter rangeCheck
  where
    bound = sizeToInt size `quot` 2
    rangeCheck (a,b)
        | a > bound || a < (-bound) = False
        | b > bound || b < (-bound) = False
        | otherwise = True

calcAndDisplayGroups :: Board -> [[(Group,Point)]]
-- | Convenience function to call `displayGroups`on the output of
-- `groupBoard`
calcAndDisplayGroups board = displayGroups board $ groupBoard board

displayGroups :: Board -> Groups -> [[(Group,Point)]]
-- | Show the groups hash in a format which is readily human readable
displayGroups board@(Board {..}) groups =
        inGroups $ sort $ zip groupsOnBoard pointsOnBoard
  where
    groupsOnBoard = catMaybes $ map (flip H.lookup groups) pointsOnBoard
    pointsOnBoard = filter (flip H.member groups) points
    points = enumeratePoints size
    inGroups = groupBy $ \a b -> (fst a == fst b)
