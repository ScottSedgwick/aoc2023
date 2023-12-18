module Dec18 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import Text.XML.HXT.DOM.Util (hexStringToInt)
import ParserUtils ( strUntil, ignore, int, string )
import Geometry ( Pt2(..), area, circumference )

type Input = [Trench]

data Trench = Trench
    { dirn :: Dirn
    , dist :: Int
    , color :: String
    } deriving stock (Show, Eq, Ord)

data Dirn = U | D | L | R deriving stock (Show, Eq, Ord)

datafile :: FilePath
datafile = "data/Day18.txt"

parser :: A.Parser Input
parser = A.many pTrench

pTrench :: A.Parser Trench
pTrench = do
    d <- pDirn
    ignore $ string " "
    l <- int
    ignore $ string " (#"
    c <- strUntil ')'
    ignore $ string ")"
    ignore A.newline
    pure $ Trench { dirn = d, dist = l, color = c }

pDirn :: A.Parser Dirn
pDirn = pd "R" R <|> pd "L" L <|> pd "U" U <|> pd "D" D

pd :: String -> Dirn -> A.Parser Dirn
pd s d = do
    ignore $ string s
    pure d


part1 :: Input -> IO Int
part1 = pure . fillArea

fillArea :: Input -> Int
fillArea xs = ps + (cs `div` 2) + 1
    where
        ls = mkPts (Pt2 0 0) xs
        ps = area ls
        cs = circumference ls

mkPts :: Pt2 -> Input -> [Pt2]
mkPts _ [] = []
mkPts p (x:xs) = p : mkPts (move p (dirn x) (dist x)) xs

move :: Pt2 -> Dirn -> Int -> Pt2
move (Pt2 c r) R x = Pt2 (c + x) r
move (Pt2 c r) L x = Pt2 (c - x) r
move (Pt2 c r) U x = Pt2 c (r - x)
move (Pt2 c r) D x = Pt2 c (r + x)


part2 :: Input -> IO Int
part2 = pure . fillArea . map fix

fix :: Trench -> Trench
fix (Trench _ _ c) = (Trench (fromInt (last c)) (hexStringToInt (init c)) c)

fromInt :: Char -> Dirn
fromInt '0' = R
fromInt '1' = D
fromInt '2' = L
fromInt '3' = U
fromInt x = error $ "Invalid direction: " <> show x