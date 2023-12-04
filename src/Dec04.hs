module Dec04 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.IntMap as I
import qualified Data.Maybe as M
import qualified Text.Trifecta as A
import ParserUtils ( ignore, string, spaces, int, intSpace )

type Input = [Game]

data Game = Game
    { num :: Int
    , ds1 :: [Int]
    , ds2 :: [Int]
    } deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day04.txt"

parser :: A.Parser Input
parser = A.many pGame

pGame :: A.Parser Game
pGame = do
    ignore $ string "Card"
    ignore spaces
    n <- int
    ignore $ string ":"
    ignore spaces
    ds1 <- A.many intSpace
    ignore $ string "|"
    ignore spaces
    ds2 <- A.many intSpace
    ignore spaces
    ignore $ (ignore A.newline <|> A.eof)
    pure $ Game { num = n, ds1 = ds1, ds2 = ds2}

part1 :: Input -> Int
part1 = sum . map score1

score1 :: Game -> Int
score1 (Game _ ds1 ds2) = if count == 0 then 0 else 2 ^ (count - 1)
    where
        count = length $ filter (\x -> x `elem` ds1) ds2

part2 :: Input -> Int
part2 gs = I.foldr (+) 0 res
    where
        m = I.fromList [(x,1) | x <- [1..length gs]]
        res = scoreAppend m gs

scoreAppend :: I.IntMap Int -> Input -> I.IntMap Int
scoreAppend m [] = m 
scoreAppend m (g:gs) = scoreAppend m' gs
    where
        p = M.maybe 0 id (I.lookup (num g) m)
        m' = updateMap p m [num g + 1 .. num g + score2 g]
    

score2 :: Game -> Int
score2 (Game _ ds1 ds2) = length $ filter (\x -> x `elem` ds1) ds2

updateMap :: Int -> I.IntMap Int -> [Int] -> I.IntMap Int
updateMap c = foldr (\a b -> I.adjust (\n -> n + c) a b)