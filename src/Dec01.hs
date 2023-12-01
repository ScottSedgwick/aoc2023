module Dec01 (Input, datafile, parser, part1, part2) where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Text.Trifecta as A
import ParserUtils (strLine, pEither, pFirst, pString, pIgnoreChar, pMatchStringAndReturn)

type Input = [String]

datafile :: FilePath
datafile = "data/Day01.txt"

parser :: A.Parser Input
parser = A.many strLine

part1 :: Input -> Int
part1 = sum . map (strToInt . firstAndLast1 . filter isDigit)

firstAndLast1 :: String -> String
firstAndLast1 [] = "0"
firstAndLast1 [x] = [x,x]
firstAndLast1 xs = [head xs, last xs]

strToInt :: String -> Int
strToInt = read

part2 :: Input -> Int
part2 = sum . map (\s -> read s :: Int) . map firstAndLast2

firstAndLast2 :: String -> String
firstAndLast2 xs = firstAndLast1 (findDigitsLR xs ++ findDigitsRL (reverse xs))

findDigitsLR :: String -> String
findDigitsLR = pEither concat (const "0") . pString (A.many digitParser)

findDigitsRL :: String -> String
findDigitsRL = pEither (reverse . concat) (const "0") . pString (A.many digitRevParser)

digits :: [(String, String)]
digits = [ ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")
         , ("1", "1"), ("2", "2"), ("3", "3"), ("4", "4"), ("5", "5"), ("6", "6"), ("7", "7"), ("8", "8"), ("9", "9") ]

revdigits :: [(String, String)]
revdigits = map (\(a,b) -> (reverse a, b)) digits

digitParser :: A.Parser String
digitParser = pFirst "" (map (\(a,b) -> pMatchStringAndReturn a b) digits) <|> pIgnoreChar

digitRevParser :: A.Parser String
digitRevParser = pFirst "" (map (\(a,b) -> pMatchStringAndReturn a b) revdigits) <|> pIgnoreChar