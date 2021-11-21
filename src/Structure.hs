import Data.Matrix hiding (trace)
import Data.Set hiding (map, foldl, filter)
import Data.Maybe
import Data.Ix
import Debug.Trace

-- Game data structure definitions
data Square = Square {
  uncovered :: Bool,
  bomb :: Bool,
  surrounding :: Maybe Int
} deriving Show

type GameMatrix = Matrix Square

gridSize = 18
corners = [(-1,0), (0,-1), (0,1), (1,0)]

-- Game algorithm declarations
testForBomb :: Int -> Int -> GameMatrix -> Bool
testForBomb i j mat = bomb $ getElem i j mat

computeSurrounding :: Int -> Int -> GameMatrix -> Int
computeSurrounding i j mat =
  foldl (\prev curr ->
    if curr /= (0,0)
    then
        fromEnum (bomb (
        fromMaybe
          (Square {uncovered = True, bomb = False, surrounding = Nothing})
          (safeGet (i + fst curr) (j + snd curr) mat)
        ))
    + prev
    else prev)
  0 (range ((-1, -1), (1, 1)))

isntUncoverable :: Square -> Bool
isntUncoverable square = bomb square || uncovered square

uncoverSquare :: Int -> Int -> GameMatrix -> Set (Int, Int) -> (GameMatrix, Set (Int, Int))
uncoverSquare i j mat discovered = do
  let center = getElem i j mat
  let bomp = bomb center
  let new = setElem (Square {
    uncovered = True,
    bomb = bomp,
    surrounding = if bomp then Nothing else Just $ computeSurrounding i j mat
  }) (i, j) mat
  let newDiscover = trace (show i ++ " " ++ show j ++ " " ++ show discovered) $ insert (i, j) discovered

  if (isntUncoverable center) then (new, empty) else trace ("") $ (fst $ foldl (\(pmat, pset) (x, y) -> uncoverSquare (i+x) (j+y) pmat pset)
    (new, newDiscover)
    (filter (\(x, y) ->
      and [
        (not $ uncovered center),
        (isJust $ safeGet (i+x) (j+y) new),
        (not $ member (i+x, j+y) newDiscover)
      ])
    corners), empty)

main :: IO ()
main = putStrLn "shart"
