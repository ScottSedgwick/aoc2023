module Dec16 (Input, datafile, parser, part1, part2) where

import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Text.Trifecta as A
import ParserUtils ( strLines )
import Geometry ( Pt2(..), Geo(col, row) )

data Space = SEmpty | SVSplit | SHSplit | SMSlash | SMBack deriving stock (Eq)

instance Show Space where
    show SEmpty  = "."
    show SVSplit = "|"
    show SHSplit = "-"
    show SMSlash = "/"
    show SMBack  = "\\"

type Input = M.Matrix Space

datafile :: FilePath
datafile = "data/Day16.txt"

parser :: A.Parser Input
parser = do
    xs <- strLines
    let ys = map (map toSpace) xs
    pure $ M.fromLists ys

toSpace :: Char -> Space
toSpace '.' = SEmpty
toSpace '|' = SVSplit
toSpace '-' = SHSplit
toSpace '/' = SMSlash
toSpace '\\' = SMBack
toSpace c = error $ "Bad char: " <> [c]

data Dirn = N | E | W | S deriving stock (Show, Eq, Ord)

part1 :: Input -> IO Int
part1 xs = do
    let ys = p1 xs ((Pt2 1 1), E) S.empty
    pure $ score ys

score :: S.Set (Pt2, Dirn) -> Int
score = S.size . S.foldr (\(p,_) b -> S.insert p b) S.empty

p1 :: Input -> (Pt2, Dirn) -> S.Set (Pt2, Dirn) -> S.Set (Pt2, Dirn)
p1 xs (p, d) s =
    if S.member (p,d) s
    then s
    else
        case M.safeGet (row p) (col p) xs of
            Nothing -> s
            Just c  ->
                let 
                    s' = S.insert (p,d) s
                in case c of
                    SEmpty  -> p1 xs (move d p) s'
                    SVSplit -> case d of
                                    N -> p1 xs (move d p) s'
                                    S -> p1 xs (move d p) s'
                                    E -> p1 xs (move S p) (p1 xs (move N p) s')
                                    W -> p1 xs (move S p) (p1 xs (move N p) s')
                    SHSplit -> case d of
                                    N -> p1 xs (move W p) (p1 xs (move E p) s')
                                    S -> p1 xs (move W p) (p1 xs (move E p) s')
                                    E -> p1 xs (move d p) s'
                                    W -> p1 xs (move d p) s'
                    SMSlash -> case d of
                                    N -> p1 xs (move E p) s'
                                    S -> p1 xs (move W p) s'
                                    E -> p1 xs (move N p) s'
                                    W -> p1 xs (move S p) s'
                    SMBack  -> case d of
                                    N -> p1 xs (move W p) s'
                                    S -> p1 xs (move E p) s'
                                    E -> p1 xs (move S p) s'
                                    W -> p1 xs (move N p) s'

move :: Dirn -> Pt2 -> (Pt2, Dirn)
move d p = 
    case d of
        N -> (p + (Pt2 0 (-1)), d)
        E -> (p + (Pt2 1 0), d)
        W -> (p + (Pt2 (-1) 0), d)
        S -> (p + (Pt2 0 1), d)

pp :: Int -> Int -> S.Set (Pt2, Dirn) -> M.Matrix Char
pp r c s = foldr (\((Pt2 y x),_) b -> M.setElem '#' (x, y) b) m s
    where
        m = M.matrix r c (\_ -> '.')

part2 :: Input -> IO Int
part2 xs = do 
    let ps = [(Pt2 1 r,E) | r <- [1 .. M.nrows xs]]
          <> [(Pt2 (M.ncols xs) r,W) | r <- [1 .. M.nrows xs]]
          <> [(Pt2 c 1,S) | c <- [1 .. M.ncols xs]]
          <> [(Pt2 c (M.nrows xs),N) | c <- [1 .. M.ncols xs]]
    print ps
    let ys = map (p2 xs) ps
    print ys
    pure $ maximum ys

p2 :: Input -> (Pt2, Dirn) -> Int
p2 xs v = score $ p1 xs v S.empty