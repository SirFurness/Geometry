module Lib where
import Control.Monad.State
import Data.Set as S
import Prelude as P

data Point = Point [Char]
               deriving (Show, Eq, Ord) --Ord is necessary of use in Set

data SubsetType = Line
            deriving (Show, Eq)

data SpaceSubset = SpaceSubset { subsetType :: SubsetType,
                                 set :: Set Point,
                                 name :: [Char]
                               }
                     deriving (Show)

type Geometry a = State (Set Point, [SpaceSubset]) a

getSubsets :: Geometry [SpaceSubset]
getSubsets = do
  universe <- get
  return $ snd universe

getPoints :: Geometry (Set Point)
getPoints = do
  universe <- get
  return $ fst universe

makeLine :: [Point] -> [Char] -> Geometry ()
makeLine givenPoints name = do
  points <- getPoints
  subsets <- getSubsets
  let newPoints = union points $ fromList givenPoints
  let line = SpaceSubset Line (fromList givenPoints) name
  let newSubsets = line : subsets
  put (newPoints, newSubsets)

isLine :: SpaceSubset -> Bool
isLine subset
  | subsetType subset == Line = True
  | otherwise = False

getLines :: Geometry [SpaceSubset]
getLines = do
  subsets <- getSubsets
  return $ P.filter isLine subsets

isCollinear :: Set Point -> Geometry Bool
isCollinear points
  | size points < 3 = return True --Note this returns true for an empty set
  | otherwise = do
      lines <- getLines
      return $ any (points `isSubsetOf`) $ P.map set lines
