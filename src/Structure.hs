module Structure where

import Data.Matrix hiding (trace)
import Data.Set hiding (map, foldl, filter, toList, delete)
import Data.List hiding (insert)
import Data.Maybe
import Data.Ix
import Debug.Trace
import Control.Applicative hiding (empty)

gridSize = 18 :: Int
bombCount = 40

-- A square can either be covered but unflagged, covered and flagged,
-- or uncovered (in which the number of surrounding bombs is visible)
data SquareState = Covered | Flagged | Uncovered Int deriving Show

isUncovered :: SquareState -> Bool
isUncovered (Uncovered x) = True
isUncovered _ = False

getSurrounding :: SquareState -> Int
getSurrounding (Uncovered sur) = sur
getSurrounding _ = 0

flag :: SquareState -> SquareState
flag Covered = Flagged
flag Flagged = Covered
flag (Uncovered sur) = (Uncovered sur)


-- The data type for a given square, with the state of the square being
-- covered and whether or not the given square has a bomb under it
data Square = Square {
  coverage :: SquareState,
  bomb :: Bool
} deriving Show

isntUncoverable :: Square -> Bool
isntUncoverable square = bomb square || (isUncovered $ coverage square)


-- The game matrix is an n x n matrix of Square elements
type GameMatrix = Matrix Square

-- Game algorithm declarations
testForBomb :: Int -> Int -> GameMatrix -> Bool
testForBomb i j mat = bomb $ getElem i j mat

hasWon :: GameMatrix -> Bool
hasWon mat = (foldl (\prev curr ->
                      prev + (fromEnum $ not (isUncovered $ coverage curr)))
              0 (toList mat)) == bombCount

hasLost :: GameMatrix -> Bool
hasLost mat = any (liftA2 (&&) (isUncovered . coverage) bomb) (toList mat)

-- This function checks the 8 adjacent squares around a given location and
-- sums up the number of bombs counted. Each adjacent square is fetched and a
-- 1 is returned if there is a bomb and a zero otherwise, then these results are
-- summed up, resulting in the total amount of bombs.
computeSurrounding :: Int -> Int -> GameMatrix -> Int
computeSurrounding i j mat =
  foldl (\prev curr ->
    if curr /= (0,0)
    then
        fromEnum (bomb (
        fromMaybe
          (Square {coverage = Covered, bomb = False})
          (safeGet (i + fst curr) (j + snd curr) mat)
        ))
    + prev
    else prev)
  0 (range ((-1, -1), (1, 1)))

-- Recursively uncovers squares from a given starting location, provided a coordinate location,
-- a game matrix, and a set of previously searched coordinates. The algorithm searches in a star
-- formation, checking each north, west, south, and east square recursively and stopping if this
-- square has already been uncovered or a bomb has been detected in an adjacent square. If we are
-- uncovering a square that has already been uncovered, we should uncover the 8 adjacent squares instead.
uncoverSquare :: (Int, Int) -> GameMatrix -> Set (Int, Int) -> (GameMatrix, Set (Int, Int))
uncoverSquare (i, j) mat discovered = do
  let center = getElem i j mat
  let bomp = bomb center
  let square = Square {
    bomb = bomp,
    coverage = if (bomp && (discovered /= empty))
                then (coverage center)
                else Uncovered $ computeSurrounding i j mat
  }
  let new = setElem square (i, j) mat
  let newDiscover = insert (i, j) discovered

  let filterand = case (coverage center) of
                    Uncovered sur -> range ((-1, -1), (1, 1))
                    _ -> [(-1,0), (0,-1), (0,1), (1,0)]

  let array = filter (\(x, y) -> and [
                --((not $ isUncovered $ coverage center) || (discovered == empty)),
                (isJust $ safeGet (i+x) (j+y) new),
                (not $ member (i+x, j+y) newDiscover)
              ]) filterand

  if (not $ isntUncoverable center) && ((getSurrounding $ coverage square) == 0)
    then
      (fst $ foldl (\(pmat, pset) (x, y) -> uncoverSquare (i+x, j+y) pmat pset)
        (new, newDiscover) array, empty)
    else (new, empty)
