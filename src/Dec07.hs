module Dec07 (Input, datafile, parser, part1, part2) where

import Data.List ( group, sort )
import qualified Text.Trifecta as A
import ParserUtils ( ignore, intLine, string, strUntil)

type Input = [Hand]

-- This is the ordering for Part 2.  For a correct answer for Part 1, move J back to between K and Q.
data Card = A | K | Q | T | Nine | Eight | Seven | Six | Five | Four | Three | Two | J deriving stock (Show, Eq, Ord)

data Hand = Hand 
    { cards :: [Card]
    , bid :: Int
    , score :: HandScore
    } deriving stock (Show, Eq)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare (Hand c1 _ s1) (Hand c2 _ s2) =
        case (compare s1 s2) of
            LT -> LT
            GT -> GT
            EQ -> compare c1 c2

data HandScore = HSFive
               | HSFour
               | HSFullHouse
               | HSThree
               | HSTwoPair
               | HSOnePair
               | HSHighCard
               deriving stock (Show, Eq, Ord)

datafile :: FilePath
datafile = "data/Day07.txt"

parser :: A.Parser Input
parser = A.many pHand

pHand :: A.Parser Hand
pHand = do
    c <- map toCard <$> strUntil ' '
    ignore $ string " "
    n <- intLine
    pure $ Hand { cards = c, bid = n, score = handScore c }

toCard :: Char -> Card
toCard 'A' = A
toCard 'K' = K
toCard 'Q' = Q
toCard 'J' = J
toCard 'T' = T
toCard '9' = Nine
toCard '8' = Eight
toCard '7' = Seven
toCard '6' = Six
toCard '5' = Five
toCard '4' = Four
toCard '3' = Three
toCard '2' = Two
toCard _ = Two

handScore :: [Card] -> HandScore
handScore c = if take 1 u == [5] then HSFive
              else if take 1 u == [4] then HSFour
              else if take 1 u == [3] && take 1 (drop 1 u) == [2] then HSFullHouse
              else if take 1 u == [3] then HSThree
              else if take 1 u == [2] && take 1 (drop 1 u) == [2] then HSTwoPair
              else if take 1 u == [2] then HSOnePair
              else HSHighCard
    where
        t = group $ sort c
        u = reverse $ sort $ map length t


part1 :: Input -> IO Int
part1 hs = do
    let ss = map bid $ reverse $ sort hs
    pure $ sum $ map (\(a,b) -> a*b) (zip [1..] ss)


part2 :: Input -> IO Int
part2 hs = do
    let ss = map bid $ reverse $ sort $ map handScore2 hs
    pure $ sum $ map (\(a,b) -> a*b) (zip [1..] ss)

handScore2 :: Hand -> Hand
handScore2 h = h { score = s1 }
    where
        c = length $ filter (\x -> x == J) (cards h)
        s1 = case (score h) of
                HSFive -> HSFive
                HSFour -> if c > 0 then HSFive else HSFour
                HSFullHouse -> if c > 0 then HSFive else HSFullHouse
                HSThree -> if c == 2 then HSFive
                           else if c > 0 then HSFour
                           else HSThree
                HSTwoPair -> if c == 2 then HSFour
                             else if c == 1 then HSFullHouse
                             else HSTwoPair
                HSOnePair -> if c > 0 then HSThree else HSOnePair
                HSHighCard -> if c > 0 then HSOnePair else HSHighCard
