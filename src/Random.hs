-- | Generate random board positions
module Random where

import Base

import Control.Arrow
import Control.Monad
import Data.HashMap.Lazy (HashMap)
import Data.List (intercalate, elemIndex, nub)
import Data.Maybe
import Data.Unique
import System.Random
import qualified Data.HashMap.Lazy as H

-- |Random instance for 'Stone' with uniform distribution
instance Random Stone where
    random = randomR (Black,White)
    randomR (lo,hi) g
        | lo == hi = (lo,g)
        | otherwise = toEnum `first` randomR (0,1) g
            where fromBool bool = if bool then Black else White

-- |Random instance for 'Size' with uniform distribution
instance Random Size where
    random = first toEnum . randomR (0,2)
    randomR (lo,hi) = first toEnum . randomR (fromEnum lo, fromEnum hi)

{-
instance Random Point where
    random = randomR ((-bound,-bound),(bound,bound))
      where bound = sizeToInt Full `quot` 2

    randomR ((xlo,ylo), (xhi,yhi)) g = ((x,y), g2)
      where
        (x,g1) = randomR (xlo,xhi)
        (y,g2) = randomR (ylo,yhi)

-}

-- |Randomly choose whether to supply a value or 'Nothing'. The weighting
-- between 'Nothing' and @Just something@ is arbitrary, and defined by
-- 'justWeight'
instance Random a => Random (Maybe a) where
    random g = intToMaybe v `first` randomR (1,justWeight) g2
      where
        (g1, g2) = split g
        v = fst $ random g1

    randomR (Just lo, Just hi) g = intToMaybe v `first` randomR (1,justWeight) g2
      where
        (g1, g2) = split g
        v = fst $ randomR (lo,hi) g1
    randomR _ g = random g

-- | The weight of a @Just something@ compared to a 'Nothing'. The weight of
-- a 'Nothing' is @1/justWeight@
justWeight = 2

intToMaybe :: v -> Int -> Maybe v
-- |Helper for the @Random Maybe@ instance
intToMaybe v i = if i == 1 then Nothing else Just v


randomBoard :: RandomGen g => g -> (Board,g)
-- |Generate a random board of any size. See also 'randomBoardAtSize'
randomBoard g = (Board {..}, g2)
    where
        (size,g1) = random g
        (layout,g2) = randomLayout size g1

randomBoardAtSize :: RandomGen g => Size -> g -> (Board,g)
-- |Generate a random board of the given size
randomBoardAtSize size g = (Board {..}, g1)
  where
    (layout,g1) = randomLayout size g

randomLayout :: RandomGen g => Size -> g -> (Layout,g)
-- |Generate a random board layout, at the given size; See also
-- 'randomBoardAtSize'
randomLayout size g = (go values, g2)
  where
    go = H.fromList . mapMaybe onlyJust . zip (enumeratePoints size)

    values :: [Maybe Stone]
    values = randoms g1
    (g1,g2) = split g

    onlyJust :: (k, Maybe v) -> Maybe (k,v)
    onlyJust (_,Nothing) = Nothing
    onlyJust (k,Just v) = Just (k,v)

    bounds = ((-bound,-bound), (bound,bound))
    bound = sizeToInt size `quot` 2
