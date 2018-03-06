module Lib where
import Control.Monad.State
import Data.Set as S
import Prelude as P

data Point = Point [Char]
               deriving (Show, Eq, Ord) --Ord is necessary for use in Set

data SubsetType = Line
            deriving (Show, Eq)

data SpaceSubset = SpaceSubset { subsetType :: SubsetType,
                                 set :: Set Point,
                                 name :: [Char]
                               }
                     deriving (Show, Eq)

type Geometry a = State (Set Point, [SpaceSubset]) a

unionSubsets :: SpaceSubset -> SpaceSubset -> Set Point
unionSubsets a b = union (set a) (set b)

getSubsets :: Geometry [SpaceSubset]
getSubsets = do
  universe <- get
  return $ snd universe

getPoints :: Geometry (Set Point)
getPoints = do
  universe <- get
  return $ fst universe

getLines :: Geometry [SpaceSubset]
getLines = do
  subsets <- getSubsets
  return $ P.filter isLine subsets

putPoints :: Set Point -> Geometry ()
putPoints points = do
  subsets <- getSubsets
  put (points, subsets)

putSubsets :: [SpaceSubset] -> Geometry ()
putSubsets subsets = do
  points <- getPoints
  put (points, subsets)


addPoints :: Set Point -> Geometry ()
addPoints points = do
  oldPoints <- getPoints
  let newPoints = union oldPoints points
  putPoints newPoints

addSpaceSubset :: SubsetType -> Set Point -> [Char] -> Geometry SpaceSubset
addSpaceSubset subsetType points name = do
  oldSubsets <- getSubsets
  let line = SpaceSubset subsetType points name
  let newSubsets = line : oldSubsets
  putSubsets newSubsets
  return line

isLineNameValid :: [Char] -> Geometry Bool
isLineNameValid lineName = do
  lines <- getLines
  return $ not $ any (lineName ==) $ P.map name lines

makeLine :: Set Point -> [Char] -> Geometry ()
makeLine points name = do
  isValidName <- isLineNameValid name
  if isValidName
    then do
      addPoints points
      newLine <- addSpaceSubset Line points name
      updateAll newLine
    else return ()

isLine :: SpaceSubset -> Bool
isLine subset
  | subsetType subset == Line = True
  | otherwise = False

isCollinear :: Set Point -> Geometry Bool
isCollinear points
  | size points < 3 = return True --Note this returns true for an empty set
  | otherwise = do
      lines <- getLines
      return $ any (points `isSubsetOf`) $ P.map set lines

updateAll :: SpaceSubset -> Geometry ()
updateAll = updateLines

updateLines :: SpaceSubset -> Geometry ()
updateLines newLine = do
  lines <- getLines
  putSubsets $ P.map (makeEqualLinesEqual newLine) lines

--If the added line is an equal line to another one, make the old line's set equal to the new one
--because the new one may contain other points that the old one did not
makeEqualLinesEqual :: SpaceSubset -> SpaceSubset -> SpaceSubset
makeEqualLinesEqual newLine oldLine
  | (set newLine) == (set oldLine) = oldLine
  | newLine `isEqualLineTo` oldLine = SpaceSubset Line (unionSubsets oldLine newLine) (name oldLine)
  | otherwise = oldLine

isEqualLineTo :: SpaceSubset -> SpaceSubset -> Bool
a `isEqualLineTo` b
  | size (intersection (set a) (set b)) >= 2 = True
  | otherwise = False
