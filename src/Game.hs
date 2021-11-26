module Main where
import Structure
import Render
import Data.Matrix hiding (fromList)
import Data.Set hiding (map, foldl, filter, toList, take)
import System.Random
import System.IO

-- this function generates count + 2 "random" numbers between 1 and 324
-- the first element will always be 0 so it is removed using tail
-- then the number range is converted into a coordinate range from
-- (1, 1) to (18, 18). After that, the provided location (where the user clicked)
-- is removed from the list to ensure the user doesn't immediately lose the game
-- (using the filter command, checking that each item is not equal to loc)
-- The remaining "count" bomb locations are then accepted out of the list
generateBombLocs :: StdGen -> CursorLocation -> Int -> Int -> [(Int, Int)]
generateBombLocs gen loc size count = take count $ filter
  (/= loc) $
  map (\l -> (
    (div (fst l) size) + 1, (mod (fst l) size) + 1)) $ tail $ take (count+2) $
    iterate (\(num, rand) -> uniformR (1::Int, 324::Int) rand) (0, gen)

gameLoop :: GameMatrix -> Maybe StdGen -> CursorLocation -> IO String
gameLoop mat started (i, j) = if (hasWon mat) then
    return "You won!"
  else if (hasLost mat) then
    return "You lost!"
  else do

  putStrLn $ renderMatrix mat (i, j)
  input <- getChar
  let square = getElem i j mat
  (case input of
    'w' -> gameLoop mat started ((max (i-1) 1), j)
    's' -> gameLoop mat started ((min (i+1) gridSize), j)
    'a' -> gameLoop mat started (i, (max (j-1) 1))
    'd' -> gameLoop mat started (i, (min (j+1) gridSize))
    'f' -> gameLoop (setElem (Square { coverage = flag $ coverage square, bomb = bomb square }) (i, j) mat) started (i, j)
    '\n' -> case started of
      Nothing -> gameLoop (fst $ uncoverSquare (i, j) mat empty) Nothing (i, j)
      Just gen -> do
        let bombs = generateBombLocs gen (i, j) gridSize bombCount
        let new = matrix gridSize gridSize (\(x, y) -> Square { coverage = Covered, bomb = member (x, y) (fromList bombs) })
        gameLoop (fst $ uncoverSquare (i, j) new empty) Nothing (i, j)
    _ -> gameLoop mat started (i, j)
    )

main :: IO ()
main = do
  g <- newStdGen
  hSetBuffering stdin NoBuffering

  -- starting uninitialized matrix that will be properly generated
  -- once user selects a square
  let mat = matrix gridSize gridSize (\(i, j) -> Square {
    coverage = Covered,
    bomb = False
  })
  exit <- gameLoop mat (Just g) (1,1)
  putStrLn exit
  --return ()
