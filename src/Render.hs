module Render where
import Structure
import Data.Matrix hiding (trace)

-- Game rendering logic
type CursorLocation = (Int, Int)

-- Renders the current game matrix as a grid of cells in a String
renderMatrix :: GameMatrix -> CursorLocation -> String
renderMatrix mat loc = "\x1b[2J\x1b[0;0H" ++ foldl (
    \prev (curr, i) -> prev ++ (foldl (
      \previ (curri, j) -> (
        previ ++ (if (i,j) == loc then "\x1b[1m" else "") ++ (renderCoverage $ coverage curri) ++ "\x1b[0m ")
      )
    "\n\n" (zip curr [1..])))
  "" (zip (toLists mat) [1..])

-- Determines the content of a rendered cell corresponding to a Square
renderCoverage :: SquareState -> String
renderCoverage (Uncovered count) = "[" ++ (if count /= 0 then (show count) else " ") ++ "]"
renderCoverage Flagged = "\x1b[31m[F]\x1b[0m"
renderCoverage Covered = "[-]"
