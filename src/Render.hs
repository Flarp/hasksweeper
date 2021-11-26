module Render where
import Structure
import Data.Matrix hiding (trace)

-- Game rendering logic
type CursorLocation = (Int, Int)

-- Renders the current game matrix as a grid of cells in a String
renderMatrix :: GameMatrix -> CursorLocation -> String
renderMatrix mat loc = "\\033c" ++ foldl (
    \prev (curr, i) -> prev ++ (foldl (
      \previ (curri, j) -> (previ ++ " " ++ (renderCoverage $ coverage curri)) )
    "\n" (zip curr [1..])))
  "" (zip (toLists mat) [1..])

-- Determines the content of a rendered cell corresponding to a Square
renderCoverage :: SquareState -> String
renderCoverage (Uncovered count) = "[" ++ (show count) ++ "]"
renderCoverage Flagged = "[F]"
renderCoverage Covered = "[-]"
