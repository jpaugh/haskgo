module Liveness where

import Base
import Group

import Control.Arrow
import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate, intersperse, elemIndex)
import Data.Maybe
import qualified Data.HashMap.Lazy as H

liveGroups :: Board -> [Group]
-- | A list of live groups from the given `Groups`
liveGroups board@(Board {..}) = liveHelper board $ explodeGroups $ groupBoard board

deadGroups :: Board -> [Group]
-- | A list of dead groups from the given `Groups`
deadGroups board = filter (`notElem` liveHelper board groups) $ H.keys groups
  where
    groups = explodeGroups $ groupBoard board

liveHelper :: Board -> Groups -> [Group]
liveHelper board@(Board {..}) groups = foldl (flip go) mempty $ H.keys groups
  where
    go group =
        let groupMembers = H.lookupDefault mempty group groups
            hasLiberty = not . all isMember . adjacentPoints size
            isMember :: Point -> Bool
            isMember k = H.member k layout
            in if any hasLiberty groupMembers then (group:) else id

-- TODO: liveStones, deadStones
