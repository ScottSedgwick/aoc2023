module Dec15 (Input, datafile, parser, part1, part2) where

import Data.Char
import qualified Data.IntMap as I
import Data.List.Split
import qualified Text.Trifecta as A
import ParserUtils

type Input = [String]

datafile :: FilePath
datafile = "data/Day15.txt"

parser :: A.Parser Input
parser = do
    s <- strLine
    pure $ splitOn "," s

part1 :: Input -> IO Int
part1 xs = do
    print $ hashs 0 "HASH"
    let ys = map (hashs 0) xs
    print ys
    pure $ sum ys

-- Determine the ASCII code for the current character of the string.
-- Increase the current value by the ASCII code you just determined.
-- Set the current value to itself multiplied by 17.
-- Set the current value to the remainder of dividing itself by 256.
hashs :: Int -> String -> Int
hashs = foldl hash

hash :: Int -> Char -> Int
hash x c = ((x + ord c) * 17) `mod` 256

type Boxes = I.IntMap [(String, Int)]

part2 :: Input -> IO Int
part2 xs = do
    let n = foldl runBox I.empty xs
    print $ I.toList n
    res <- score (I.toList n)
    pure res

runBox :: Boxes -> String -> Boxes
runBox m s = if last s == '-'
             then runRemove m (init s)
             else runAdd m (splitOn "=" s)

runRemove :: Boxes -> String -> Boxes
runRemove m s = I.alter (\mxs -> case mxs of
                                   Nothing -> Nothing
                                   Just xs -> removeLens s xs) (box s) m

removeLens :: String -> [(String, Int)] -> Maybe [(String, Int)]
removeLens l xs = if xs' == [] then Nothing else Just xs'
    where
        xs' = removeLens' l xs

removeLens' :: String -> [(String, Int)] -> [(String, Int)]
removeLens' _ [] = []
removeLens' s ((l,f):xs) = if s == l then xs else (l,f):(removeLens' s xs)

runAdd :: Boxes -> [String] -> Boxes
runAdd m ss = I.alter (\mxs -> case mxs of
                                 Nothing -> Just [(a, b)]
                                 Just xs -> Just (addLens (a, b) xs)) (box a) m
    where
        a = head ss
        b = toi $ head (tail ss)

addLens :: (String, Int) -> [(String, Int)] -> [(String, Int)]
addLens (l,f) [] = [(l,f)]
addLens (l,f) ((l',f'):xs) = if l == l' then (l,f):xs else (l',f'):(addLens (l,f) xs)


box :: String -> Int
box = hashs 0

toi :: String -> Int
toi = read

score :: [(Int, [(String, Int)])] -> IO Int
score xs = do
    let ys = map (\(n,b) -> (n + 1) * score' b) xs
    print ys
    pure $ sum ys

score' :: [(String, Int)] -> Int
score' xs = sum zs
    where
        ys = zip (map snd xs) [1..]
        zs = map (\(a,b) -> a * b) ys