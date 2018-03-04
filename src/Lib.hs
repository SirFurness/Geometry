module Lib where
import Data.Set

data Point = Point [Char]
               deriving (Show, Eq, Ord)
universal = fromList [[Point "a", Point "b", Point "c", Point "d"], []]
data Line = Line (Set Point)

class SpaceSubset a where
  getSet :: a -> Set Point

instance SpaceSubset Line where
  getSet (Line set) = set

isCollinear :: [Point] -> Bool
