module Game where
import Structure
import Render
import Data.Matrix
import System.Random
import Control.Monad.State

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

main :: IO () -> ()
main = do
  --g <- newStdGen
  --let
  return ()
