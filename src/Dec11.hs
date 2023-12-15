module Dec11 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A
import ParserUtils

datafile :: FilePath
datafile = "data/Day11.txt"

data Input = Input
    { rows :: Int
    , cols :: Int
    , stars :: [(Int, Int)]
    } deriving stock (Show, Eq)

parser :: A.Parser Input
parser = do
    rs <- strLines
    let ss = convertRows 0 rs
    pure $ Input { rows = length rs, cols = length (head rs), stars = ss }

convertRows :: Int -> [String] -> [(Int, Int)]
convertRows _ [] = []
convertRows n (r:rs) = convertRow n 0 r ++ convertRows (n + 1) rs

convertRow :: Int -> Int -> String -> [(Int, Int)]
convertRow _ _ [] = []
convertRow r c (x:xs) = 
    if x == '#'
        then (r,c) : convertRow r (c + 1) xs
        else convertRow r (c + 1) xs

-- 9177603
part1 :: Input -> IO Int
part1 xs = do
    -- Expand
    let ys = expand 1 xs
    -- Generate list of pairs
    let zs = pairs (stars ys)
    -- Sum of Hamming distance
    let ds = map (\((x1, y1), (x2, y2)) -> (abs(x2 - x1) + abs(y2 - y1))) zs
    pure $ sum ds

expand :: Int -> Input -> Input
expand n (Input r c xs) = (Input r c zs)
    where
        ys = expandRows n (r - 1) xs
        zs = expandCols n (c - 1) ys

expandRows :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
expandRows _ 0 xs = xs
expandRows n r xs =
    if any (\(x,_) -> x == r) xs
        then expandRows n (r - 1) xs
        else expandRows n (r - 1) (expandAbove n r xs)

expandAbove :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
expandAbove n r xs = map f xs
    where 
        f (x,y) = if x > r then (x + n, y) else (x, y)

expandCols :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
expandCols _ 0 xs = xs
expandCols n c xs =
    if any (\(_,y) -> y == c) xs
        then expandCols n (c - 1) xs
        else expandCols n (c - 1) (expandRight n c xs)

expandRight :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
expandRight n c xs = map f xs
    where 
        f (x,y) = if y > c then (x, y + n) else (x, y)

pairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairs [] = []
pairs [_] = []
pairs (x:xs) = zip (repeat x) xs ++ pairs xs


part2 :: Input -> IO Int
part2 xs = do
    -- Expand
    let ys = expand 999999 xs
    print ys
    -- Generate list of pairs
    let zs = pairs (stars ys)
    print zs
    print $ length zs
    -- Sum of Hamming distance
    let ds = map (\((x1, y1), (x2, y2)) -> (abs(x2 - x1) + abs(y2 - y1))) zs
    print ds
    pure $ sum ds