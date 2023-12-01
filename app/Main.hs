module Main (main) where

import qualified Text.Trifecta as A
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import ParserUtils (prtParserError, pEither)

import Dec02 (Input, datafile, parser, part1, part2)

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
    print $ part1 ys
    et1 <- getCurrentTime
    putStrLn $ "Time taken: " <> show (diffUTCTime et1 st1) <> " secs."
    
    st2 <- getCurrentTime
    putStrLn "Part Two: "
    print $ part2 ys
    et2 <- getCurrentTime
    putStrLn $ "Time taken: " <> show (diffUTCTime et2 st2) <> " secs."
