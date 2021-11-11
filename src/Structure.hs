import Data.Matrix hiding (trace)
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

-- combine results from the uncover algorithm together to get one result
unionMats :: [GameMatrix] -> GameMatrix
unionMats mats = matrix gridSize gridSize $ \(i, j) ->
  Square {
    uncovered = (or $ map (uncovered . getElem i j) mats),
    bomb = bomb $ getElem i j $ head mats,
    surrounding = foldl (\prev curr ->
      if (isJust curr)
        then curr
        else prev
    ) Nothing (map (surrounding . getElem i j) mats)
  }

uncoverSquare :: Int -> Int -> GameMatrix -> GameMatrix
uncoverSquare i j mat = do
  let center = getElem i j mat
  let bomp = bomb center
  let new = setElem (Square {
    uncovered = True,
    bomb = bomp,
    surrounding = if bomp then Nothing else Just $ computeSurrounding i j mat
  }) (i, j) mat

  if ((||) bomp (uncovered center)) then new else unionMats $ map (\(x, y) -> uncoverSquare (i+x) (j+y) new)
    $ trace (show i ++ " " ++ show j ++ show (uncovered center) ++ show (safeGet i+1 j+1 new)) (filter (\(x, y) -> isJust $ safeGet (i+x) (j+y) new) corners)

main :: IO ()
main = putStrLn "shart"
