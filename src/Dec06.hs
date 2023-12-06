module Dec06 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A
import ParserUtils

-- Time, Distance
type Input = [(Int, Int)]

datafile :: FilePath
datafile = "data/Day06.txt"

parser :: A.Parser Input
parser = do
    ignore $ string "Time:"
    ignore spaces
    xs <- A.many intSpace
    ignore eol
    ignore $ string "Distance:"
    ignore spaces
    ys <- A.many intSpace
    pure $ zip xs ys

part1 :: Input -> IO Int
part1 xs = do
    let ys = map winCount xs
    print ys
    pure $ product ys

winCount :: (Int, Int) -> Int
winCount (t, d) = length ws
    where
        ts = [1..t]
        ds = map (dist t) ts
        ws = filter (\d' -> d' > d) ds 

dist :: Int -> Int -> Int
dist t ta = (t - ta) * ta

part2 :: Input -> IO Int
part2 xs = do
    let ts = map show $ map fst xs
    print ts
    let t = read (concat ts) :: Int
    print t
    let ds = map show $ map snd xs
    print ds
    let d = read (concat ds) :: Int
    print d
    pure $ winCount (t, d)