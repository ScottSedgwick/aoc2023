module Dec03 (Input, datafile, parser, part1, part2) where

import qualified Text.Trifecta as A
import Data.Char ( digitToInt, isDigit )
import Data.Maybe ( catMaybes )
import ParserUtils ( strLine )
import Geometry ( Pt2(..) )

type Input = ([Number], [Symbol])

data Number = Number
    { value :: Int
    , tl :: Pt2
    , br :: Pt2
    } deriving stock (Show, Eq)

data Symbol = Symbol
    { sym :: Char
    , pos :: Pt2
    } deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day03.txt"

parser :: A.Parser Input
parser = do
    xs <- A.many strLine
    let ys = symbols (Pt2 0 0) xs
    let zs = numbers (Pt2 0 0) xs
    pure (zs, ys)

symbols :: Pt2 -> [String] -> [Symbol]
symbols _ [] = []
symbols (Pt2 x _) (z:zs) = symbolLine (Pt2 x 0) z ++ symbols (Pt2 (x + 1) 0) zs

symbolLine :: Pt2 -> String -> [Symbol]
symbolLine _ [] = []
symbolLine (Pt2 x y) (z:zs) =
    if isSymbol' z
        then (Symbol z (Pt2 x y)) : symbolLine (Pt2 x (y + 1)) zs
        else symbolLine (Pt2 x (y + 1)) zs

isSymbol' :: Char -> Bool
isSymbol' c = not (isDigit c) && (c /= '.')

numbers :: Pt2 -> [String] -> [Number]
numbers _ [] = []
numbers (Pt2 x y) (z:zs) = n1Line (Pt2 x y) z ++ numbers (Pt2 (x + 1) y) zs

n1Line :: Pt2 -> String -> [Number]
n1Line _ [] = []
n1Line (Pt2 x y) (z:zs) = if isDigit z then n : rest1 else rest2
    where
        (n, Pt2 _ y', zs') = n2Line (digitToInt z) (Pt2 x y) (Pt2 x y) zs
        rest1 = n1Line (Pt2 x (y' + 1)) zs'
        rest2 = n1Line (Pt2 x (y + 1)) zs

n2Line :: Int -> Pt2 -> Pt2 -> String -> (Number, Pt2, String)
n2Line n p0 p [] = (Number { value = n, tl = p0, br = p }, p, [])
n2Line n p0 (Pt2 x y) (z:zs) =
    if isDigit z
        then n2Line (n * 10 + (digitToInt z)) p0 (Pt2 x (y + 1)) zs
        else (Number { value = n, tl = p0, br = (Pt2 x y) }, Pt2 x y, (z:zs))


part1 :: Input -> Int
part1 (ns, ss) = sum (map value xs) 
    where
        xs = filter (isAdjacent ss) ns

isAdjacent :: [Symbol] -> Number -> Bool
isAdjacent ss n = any (isAdjacent' n) ss

isAdjacent' :: Number -> Symbol -> Bool
isAdjacent' n (Symbol _ (Pt2 x y)) = xok && yok
    where 
        (Pt2 x1 y1) = tl n
        (Pt2 x2 y2) = br n
        yok = (y >= y1 - 1) && (y <= y2 + 1)
        xok = (x >= x1 - 1) && (x <= x2 + 1)


part2 :: Input -> Int
part2 (ns, ss) = sum $ map prods ps
    where
        cs = filter (\(Symbol c _) -> c == '*') ss
        ps = catMaybes $ map (findPairs ns) cs

prods :: (Number, Number) -> Int
prods ((Number x _ _), (Number y _ _ )) = x * y

findPairs :: [Number] -> Symbol -> Maybe (Number, Number)
findPairs ns s = takePair xs
    where
        xs = filter (flip isAdjacent' s) ns

takePair :: [Number] -> Maybe (Number, Number)
takePair [] = Nothing
takePair (_:[]) = Nothing
takePair (x:y:_) = Just (x,y)