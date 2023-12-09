module Dec09 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A
import ParserUtils ( eol, ignore, intSpace )

type Input = [[Int]]

datafile :: FilePath
datafile = "data/Day09.txt"

parser :: A.Parser Input
parser = A.many pIntLine

pIntLine :: A.Parser [Int]
pIntLine = do
    xs <- A.many intSpace
    ignore eol
    pure xs


part1 :: Input -> IO Int
part1 = pure . sum . map extend

extend :: [Int] -> Int
extend [] = 0
extend xs = if all (==0) xs then 0 
            else last xs + extend (diffs xs)

diffs :: [Int] -> [Int]
diffs (x:y:zs) = y - x : diffs (y:zs)
diffs _ = []


part2 :: Input -> IO Int
part2 = pure . sum . map backward

backward :: [Int] -> Int
backward [] = 0
backward (x:xs) = if all (==0) (x:xs) then 0 
                  else x - backward (diffs (x:xs))