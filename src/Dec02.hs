module Dec02 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import qualified Text.Trifecta as A
import ParserUtils ( eol, ignore, int, string )

datafile :: FilePath
datafile = "data/Day02.txt"

data Color = Blue | Red | Green deriving stock (Show, Eq)

data Token = Token
    { count :: Int
    , color :: Color
    } deriving stock (Show, Eq)

data Game = Game 
    { number :: Int
    , tokens :: [Token]
    } deriving stock (Show, Eq)

type Input = [Game]

parser :: A.Parser Input
parser = A.many pGame

pGame :: A.Parser Game
pGame = do
    _ <- string "Game "
    n <- int
    _ <- string ": "
    ts <- A.many pToken
    pure $ Game { number = n, tokens = ts }

pToken :: A.Parser Token
pToken = do
    n <- int
    _ <- string " "
    c <- pColor
    _ <- (ignore $ string ", ") <|> (ignore $ string "; ") <|> (ignore $ eol)
    pure $ Token { count = n, color = c }

pColor :: A.Parser Color
pColor = pBlue <|> pGreen <|> pRed

pBlue :: A.Parser Color
pBlue = do
    _ <- string "blue"
    pure Blue

pGreen :: A.Parser Color
pGreen = do
    _ <- string "green"
    pure Green

pRed :: A.Parser Color
pRed = do
    _ <- string "red"
    pure Red


part1 :: Input -> Int
part1 xs = sum $ map (\g -> number g) $ filter possible xs

possible :: Game -> Bool
possible gm = r <= 12 && g <= 13 && b <= 14
    where
        (r, g, b) = tMaxes (tokens gm)

tMax :: Color -> [Token] -> Int
tMax c ts = maximum $ map (\t -> count t) $ filter (\t -> color t == c) ts

tMaxes :: [Token] -> (Int, Int, Int)
tMaxes ts = (tMax Red ts, tMax Green ts, tMax Blue ts)


part2 :: Input -> Int
part2 = sum . map powers

powers :: Game -> Int
powers gm = r * b * g
    where
        (r, g, b) = tMaxes (tokens gm)
