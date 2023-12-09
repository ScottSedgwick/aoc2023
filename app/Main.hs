module Main (main) where

import qualified Text.Trifecta as A
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import ParserUtils (prtParserError, pEither)

import Dec09 (Input, datafile, parser, part1, part2)

main :: IO ()
main = do
    xs <- readFile datafile
    let ys = A.parseString parser mempty xs
    pEither prtResult prtParserError ys

prtResult :: Input -> IO()
prtResult ys = do
    putStrLn "Data: "
    print ys

    st1 <- getCurrentTime
    putStrLn "Part One: "
    p1 <- part1 ys
    print p1
    et1 <- getCurrentTime
    putStrLn $ "Time taken: " <> show (diffUTCTime et1 st1) <> " secs."
    
    st2 <- getCurrentTime
    putStrLn "Part Two: "
    p2 <- part2 ys
    print p2
    et2 <- getCurrentTime
    putStrLn $ "Time taken: " <> show (diffUTCTime et2 st2) <> " secs."
