-- |Dummy main. This module makes a great entry point for *GHCI*, because
-- it imports nearly everything else. Otherwise, it serves no purpose
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

-- | Dummy main
main = putStrLn "Hello, World!"
