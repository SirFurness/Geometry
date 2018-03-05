{-# LANGUAGE ExistentialQuantification #-}
module Lib where
import Control.Monad.State
import Data.Set

data Point = Point [Char]
               deriving (Show, Eq, Ord)
-- use for input to state:(fromList [Point "a", Point "b", Point "c", Point "d"], [])
data SpaceSubset = forall s. (SubsetOfSpace s, Show s) => SpaceSubset s
type SpaceWithSubsets = (Set Point, [SpaceSubset])
type Geometry a = State SpaceWithSubsets a

data Line = Line (Set Point)
  deriving (Show, Eq)

class SubsetOfSpace a where
  getSet :: a -> Set Point
instance Show SpaceSubset where
  show (SpaceSubset a) = show a
instance SubsetOfSpace SpaceSubset where
  getSet (SpaceSubset a) = getSet a

instance SubsetOfSpace Line where
  getSet (Line set) = set

getSubsets :: Geometry [SpaceSubset]
getSubsets = do
  universe <- get
  return $ snd universe

getPoints :: Geometry (Set Point)
getPoints = do
  universe <- get
  return $ fst universe

--isCollinear :: [Point] -> Geometry Bool
--isCollinear points = do
  
