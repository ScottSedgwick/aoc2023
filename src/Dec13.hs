module Dec13 (Input, datafile, parser, part1, part2) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe
import qualified Text.Trifecta as A
import ParserUtils

type Input = [M.Matrix Char]

datafile :: FilePath
datafile = "data/Day13.txt"

parser :: A.Parser Input
parser = do 
    xs <- A.many pGroup
    pure $ map M.fromLists xs

pGroup :: A.Parser [String]
pGroup = do
    xs <- strLines
    ignore A.newline
    pure xs

data Reflect = RVert Int  -- # cols to left
             | RHorz Int  -- 100 * # rows above
             | RNone      -- 0
             deriving stock (Show, Eq)

part1 :: Input -> IO Int
part1 xs = do
    let ys = map (findReflection 0 0) xs
    print ys
    pure $ sum $ map valueReflection ys

findReflection :: Int -> Int -> M.Matrix Char -> Reflect
findReflection mistakes allowed m = 
    case findV mistakes allowed 1 m of
        (Just r) -> r
        Nothing  -> 
            case findH mistakes allowed 1 m of
                (Just r) -> r
                Nothing  -> RNone

findV :: Int -> Int -> Int -> M.Matrix Char -> Maybe Reflect
findV mistakes allowed n m = 
    if isNothing (M.safeGet 1 n m) || isNothing (M.safeGet 1 (n + 1) m)
    then Nothing    -- n is outside bounds of matrix
    else 
        case findV' mistakes allowed n 0 m of
            Just v  -> Just v      -- found a reflection
            Nothing -> findV mistakes allowed (n+1) m 

findV' :: Int -> Int -> Int -> Int -> M.Matrix Char -> Maybe Reflect
findV' mistakes allowed n dn m = 
    if mistakes > allowed 
    then Nothing
    else case M.safeGetCol n1 m of
        Nothing -> if mistakes == allowed 
                   then Just (RVert n)
                   else Nothing 
        Just c1 -> 
            case M.safeGetCol n2 m of
                Nothing -> if mistakes == allowed
                           then Just (RVert n)
                           else Nothing 
                Just c2 -> if c1 == c2 
                           then findV' mistakes allowed n (dn + 1) m
                           else findV' (mistakes + differences c1 c2) allowed n (dn + 1) m
    where
        n1 = n - dn
        n2 = n + dn + 1

findH :: Int -> Int -> Int -> M.Matrix Char -> Maybe Reflect
findH mistakes allowed n m = 
    if isNothing (M.safeGet n 1 m) || isNothing (M.safeGet (n + 1) 1 m)
    then Nothing
    else 
        case findH' mistakes allowed n 0 m of
            Just h  -> Just h
            Nothing -> findH mistakes allowed (n + 1) m

findH' :: Int -> Int -> Int -> Int -> M.Matrix Char -> Maybe Reflect
findH' mistakes allowed n dn m =
    if mistakes > allowed
    then Nothing
    else case M.safeGetRow n1 m of
        Nothing -> if mistakes == allowed
                   then Just (RHorz n)
                   else Nothing
        Just h1 -> 
            case M.safeGetRow n2 m of
                Nothing -> if mistakes == allowed
                           then Just (RHorz n)
                           else Nothing
                Just h2 -> if h1 == h2
                           then findH' mistakes allowed n (dn + 1) m
                           else findH' (mistakes + differences h1 h2) allowed n (dn + 1) m
    where
        n1 = n - dn
        n2 = n + dn + 1

differences :: V.Vector Char -> V.Vector Char -> Int
differences v1 v2 = V.sum $ V.zipWith (\a b -> if a == b then 0 else 1) v1 v2

valueReflection :: Reflect -> Int
valueReflection (RVert x) = x
valueReflection (RHorz x) = 100 * x
valueReflection RNone = 0

part2 :: Input -> IO Int
part2 xs = do
    let ys = map (findReflection 0 1) xs
    print ys
    pure $ sum $ map valueReflection ys