import Data.Text as T
import Data.Text.IO as TIO

import Data.List as L
import Debug.Trace as DT

data Point = Point Integer Integer deriving (Show, Eq, Ord)
data Line = Line Point Point deriving (Show)

parseCell :: Text -> (Char, Integer)
parseCell c = (T.head c, (read . unpack . T.tail) c)

parseLine :: Text -> [Line]
parseLine l = L.tail $ L.map normalize $ L.scanl expand (Line centerPoint centerPoint) cells
  where cells = parseCell <$> T.splitOn (T.pack ",") l
        expand (Line _ p2) c = Line p2 (endPoint p2 c)

endPoint :: Point -> (Char, Integer) -> Point
endPoint (Point x y) ('U', n) = Point x (y+n)
endPoint (Point x y) ('R', n) = Point (x+n) y
endPoint (Point x y) ('D', n) = Point x (y-n)
endPoint (Point x y) ('L', n) = Point (x-n) y

intersecting :: Line -> Line -> Bool
intersecting (Line (Point x1 y1) (Point x2 y2))
  (Line (Point x3 y3) (Point x4 y4)) =
    (x1 <= x3 && x3 <= x2 && y3 <= y1 && y1 <= y4) ||
    (y1 <= y3 && y3 <= y2 && x3 <= x1 && x1 <= x4)

intersection :: Line -> Line -> [Point]
intersection l1 l2 = L.map fst $ L.filter (pairApply (==)) $ carthesian (toPoints l1) (toPoints l2)
  where toPoints (Line (Point x1 y1) (Point x2 y2)) = [Point a b | a <- [x1..x2], b <- [y1..y2]]

centerPoint = Point 0 0

normalize :: Line -> Line
normalize l@(Line (Point x1 y1) (Point x2 y2))
  | x1 > x2 = Line (Point x2 y2) (Point x1 y1)
  | y1 > y2 = Line (Point x2 y2) (Point x1 y1)
  | otherwise = l

manhattanDistance :: Point -> Integer
manhattanDistance (Point x y) = abs x + abs y

solution :: Text -> Text -> Integer
solution l1 l2 = (L.head . L.tail . L.sort) distances
  where distances = L.map manhattanDistance intersections
        intersections = L.concatMap (pairApply intersection) intersectingLines
        intersectingLines = L.filter (pairApply intersecting) $ carthesian lines1 lines2
        lines1 = parseLine l1
        lines2 = parseLine l2

pairApply :: (a -> b -> c) -> (a, b) -> c
pairApply f (a, b) = f a b

carthesian :: [a] -> [b] -> [(a, b)]
carthesian x y = [(a, b) | a <- x, b <- y]

main :: IO ()
main = do
  content <- TIO.readFile "input"

  let lines = T.lines content
  -- let lines = T.pack <$> ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"]
  -- let lines = T.pack <$> ["R8,U5,L5,D3", "U7,R6,D4,L4"]
  -- let lines = T.pack <$> ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]

  Prelude.putStrLn (show $ solution (L.head lines) (L.last lines))
  return ()
