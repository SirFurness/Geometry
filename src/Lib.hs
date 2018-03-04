module Lib where
import qualified Data.Set as S

data Point = Point [Char]
               deriving (Show, Eq, Ord)
data Space = Space (S.Set Point)
               deriving (Show, Eq, Ord)
data Line = Line (S.Set Point)
               deriving (Show, Eq, Ord)

class Intersectable a where
  intersection :: a -> a -> S.Set Point

instance Intersectable Line where
  intersection (Line set) (Line set2) = set `S.intersection` set2
