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

instance Random Piece where
    random = randomR (Black,White)
    randomR (lo,hi) g
        | lo == hi = (lo,g)
        | otherwise = toEnum `first` randomR (0,1) g
            where fromBool bool = if bool then Black else White

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

instance Random a => Random (Maybe a) where
    random g = intToMaybe v `first` randomR (0,justWeight) g2
      where
        (g1, g2) = split g
        v = fst $ random g1

    randomR (Just lo, Just hi) g = intToMaybe v `first` randomR (0,justWeight) g2
      where
        (g1, g2) = split g
        v = fst $ randomR (lo,hi) g1
    randomR _ g = random g

justWeight = 1

intToMaybe :: v -> Int -> Maybe v
intToMaybe v i = if i == 1 then Nothing else Just v

randomBoard :: RandomGen g => g -> (Board,g)
randomBoard g = (Board {..}, g2)
    where
        (size,g1) = random g
        (layout,g2) = randomLayout size g1

randomBoardAtSize :: RandomGen g => Size -> g -> (Board,g)
randomBoardAtSize size g = (Board {..}, g1)
  where
    (layout,g1) = randomLayout size g

randomLayout :: RandomGen g => Size -> g -> (Layout,g)
randomLayout size g = (go values, g2)
  where
    go = H.fromList . mapMaybe onlyJust . zip (enumeratePoints size)

    values :: [Maybe Piece]
    values = randoms g1
    (g1,g2) = split g

    onlyJust :: (k, Maybe v) -> Maybe (k,v)
    onlyJust (_,Nothing) = Nothing
    onlyJust (k,Just v) = Just (k,v)

    bounds = ((-bound,-bound), (bound,bound))
    bound = sizeToInt size `quot` 2
