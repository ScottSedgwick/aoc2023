module Dec08 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import qualified Text.Trifecta as A
import ParserUtils ( ignore, string, eol, strUntil )

type Input = Path

data Path = Path
    { dirs :: [Dir] 
    , steps :: M.Map String (String, String)
    } deriving stock (Show, Eq)

data Dir = R | L deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day08.txt"

parser :: A.Parser Input
parser = do
    ds <- A.many pDir
    ignore eol
    ignore eol
    ss <- A.many pStep
    pure $ Path { dirs = ds, steps = M.fromList ss }

pDir :: A.Parser Dir
pDir = pLeft <|> pRight

pLeft :: A.Parser Dir
pLeft = do
    ignore $ A.char 'L'
    pure L

pRight :: A.Parser Dir
pRight = do
    ignore $ A.char 'R'
    pure R

pStep :: A.Parser (String, (String, String))
pStep = do
    k <- strUntil ' '
    ignore $ string " = ("
    l <- strUntil ','
    ignore $ string ", "
    r <- strUntil ')'
    ignore $ string ")"
    ignore $ (ignore eol) <|> A.eof
    pure $ (k, (l, r))

-- 18827
part1 :: Input -> IO Int
part1 input = p1 0 "AAA" (concat $ repeat $ dirs input) (steps input)

p1 :: Int -> String -> [Dir] -> M.Map String (String, String) -> IO Int
p1 _ _ [] _ = error "Run out of directions"
p1 x k (d:ds) m = do
    if k == "ZZZ" then pure x
    else
        case M.lookup k m of
            Nothing -> error $ "Key not found: " <> k
            Just (l,r) -> case d of
                L -> p1 (x + 1) l ds m
                R -> p1 (x + 1) r ds m


-- 20220305520997
part2 :: Input -> IO Int
part2 (Path steps nodes) = do
    let steps' = cycle steps
    pure $ (foldl1 lcm [ pathLength p2 nodes steps' start
                       | start <- M.keys nodes, last start == 'A'])

p2 :: [Char] -> Bool
p2 x = 'Z' == last x

pathLength :: (String -> Bool) -> M.Map String (String, String) -> [Dir] -> String -> Int
pathLength p nodes = go 0
  where
    go :: Int -> [Dir] -> String -> Int
    go n [] _ = n
    go n (dir : dirs) here
      | p here = n
      | otherwise =
      case (dir, nodes M.! here) of
        (L, (l, _)) -> go (n + 1) dirs l
        (R, (_, r)) -> go (n + 1) dirs r
