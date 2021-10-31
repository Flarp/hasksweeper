import Data.Matrix
import Data.Maybe
import Data.Ix

-- Game data structure definitions
data Square = Square {
  uncovered :: Bool,
  bomb :: Bool,
  surrounding :: Maybe Int
} deriving Show

type GameMatrix = Matrix Square

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

main :: IO ()
main = putStrLn "shart"
