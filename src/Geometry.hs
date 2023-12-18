module Geometry (Pt2(..), Pt3(..), Geo(..), area, circumference) where

import Data.Ix ( Ix(inRange, range, index) )
import GHC.Ix (Ix(unsafeIndex, unsafeRangeSize), indexError)

class Geo a where
    manhattan :: a -> a -> Int
    bounds :: [a] -> (a, a)
    neighbours :: a -> [a]
    row :: a -> Int
    col :: a -> Int

data Pt2 = Pt2 !Int !Int deriving stock (Read, Show, Ord, Eq)

instance Geo Pt2 where
    manhattan :: Pt2 -> Pt2 -> Int
    manhattan (Pt2 r1 c1) (Pt2 r2 c2) = abs (r2 - r1) + abs (c2 - c1)
    bounds :: [Pt2] -> (Pt2, Pt2)
    bounds ps = (Pt2 (minimum rs) (minimum cs), Pt2 (maximum rs) (maximum cs))
      where
        cs = map col ps
        rs = map row ps
    neighbours :: Pt2 -> [Pt2]
    neighbours (Pt2 r c) = [Pt2 (r - 1) c, Pt2 (r + 1) c, Pt2 r (c - 1), Pt2 r (c + 1)]
    row :: Pt2 -> Int
    row (Pt2 _ r) = r
    col :: Pt2 -> Int
    col (Pt2 c _) = c

instance Num Pt2 where
    (+) :: Pt2 -> Pt2 -> Pt2
    (+) (Pt2 r1 c1) (Pt2 r2 c2) = Pt2 (r1 + r2) (c1 + c2)
    (*) :: Pt2 -> Pt2 -> Pt2
    (*) (Pt2 r1 c1) (Pt2 r2 c2) = Pt2 (r1 * r2) (c1 * c2)
    abs :: Pt2 -> Pt2
    abs (Pt2 r c) = Pt2 (abs r) (abs c)
    signum :: Pt2 -> Pt2
    signum (Pt2 r c) = Pt2 (signum r) (signum c)
    negate :: Pt2 -> Pt2
    negate (Pt2 r c) = Pt2 (negate r) (negate c)
    fromInteger :: Integer -> Pt2
    fromInteger x = Pt2 (fromInteger x) (fromInteger x)

instance Ix Pt2 where
  unsafeIndex :: (Pt2, Pt2) -> Pt2 -> Int
  unsafeIndex (Pt2 lorow locol, Pt2 hirow hicol) (Pt2 row2 col2) =
    unsafeIndex (lorow,hirow) row2 * unsafeRangeSize (locol,hicol) + unsafeIndex (locol,hicol) col2
  {-# INLINE unsafeIndex #-}

  index :: (Pt2, Pt2) -> Pt2 -> Int
  index b i
    | inRange b i = unsafeIndex b i
    | otherwise   = indexError b i "Coord"
  {-# INLINE index #-}

  inRange :: (Pt2, Pt2) -> Pt2 -> Bool
  inRange (Pt2 lorow locol, Pt2 hirow hicol) (Pt2 row2 col2) =
    inRange (lorow,hirow) row2 && inRange (locol,hicol) col2
  {-# INLINE inRange #-}

  range :: (Pt2, Pt2) -> [Pt2]
  range (Pt2 lorow locol, Pt2 hirow hicol) =
    [Pt2 row2 col2 | row2 <- [lorow..hirow], col2 <- [locol..hicol]]
  {-# INLINE range #-}

  unsafeRangeSize :: (Pt2, Pt2) -> Int
  unsafeRangeSize (Pt2 lorow locol, Pt2 hirow hicol) =
    (hirow - lorow + 1) * (hicol - locol + 1)
  {-# INLINE unsafeRangeSize #-}

area :: [Pt2] -> Int
area xs = (abs (sum zs)) `div` 2
  where
    ys = zip xs (drop 1 $ cycle xs)
    zs = map f ys
    f ((Pt2 y0 x0),(Pt2 y1 x1)) = x0 * y1 - x1 * y0

circumference :: [Pt2] -> Int
circumference xs = sum (map f (zip xs (drop 1 $ cycle xs)))
  where
    f (p1, p2) = manhattan p1 p2

data Pt3 = Pt3 !Int !Int !Int deriving stock (Read, Show, Ord, Eq)

instance Geo Pt3 where
    manhattan :: Pt3 -> Pt3 -> Int
    manhattan (Pt3 x1 y1 z1) (Pt3 x2 y2 z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)
    bounds :: [Pt3] -> (Pt3, Pt3)
    bounds ps = (Pt3 (minimum xs) (minimum ys) (minimum zs), Pt3 (maximum xs) (maximum ys) (maximum zs))
      where
        xs = map col ps
        ys = map row ps
        zs = map (\(Pt3 _ _ z) -> z) ps
    neighbours :: Pt3 -> [Pt3]
    neighbours (Pt3 x y z) = [Pt3 (x - 1) y z, Pt3 (x + 1) y z, Pt3 x (y - 1) z, Pt3 x (y + 1) z, Pt3 x y (z - 1), Pt3 x y (z + 1)]
    row :: Pt3 -> Int
    row (Pt3 _ y _) = y
    col :: Pt3 -> Int
    col (Pt3 x _ _) = x

instance Num Pt3 where
    (+) :: Pt3 -> Pt3 -> Pt3
    (+) (Pt3 x1 y1 z1) (Pt3 x2 y2 z2) = Pt3 (x1 + x2) (y1 + y2) (z1 + z2)
    (*) :: Pt3 -> Pt3 -> Pt3
    (*) (Pt3 x1 y1 z1) (Pt3 x2 y2 z2) = Pt3 (x1 * x2) (y1 * y2) (z1 * z2)
    abs :: Pt3 -> Pt3
    abs (Pt3 x y z) = Pt3 (abs x) (abs y) (abs z)
    signum :: Pt3 -> Pt3
    signum (Pt3 x y z) = Pt3 (signum x) (signum y) (signum z)
    fromInteger :: Integer -> Pt3
    fromInteger w = Pt3 (fromIntegral $ w `div` 1000000) (fromIntegral $ w `div` 1000 `mod` 1000) (fromIntegral $ w `mod` 1000)
    negate :: Pt3 -> Pt3
    negate (Pt3 x y z) = Pt3 (x * (-1)) (y * (-1)) (z * (-1))

instance Ix Pt3 where
    range :: (Pt3, Pt3) -> [Pt3]
    range ((Pt3 x1 y1 z1), (Pt3 x2 y2 z2)) = [Pt3 x y z | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
    inRange :: (Pt3, Pt3) -> Pt3 -> Bool
    inRange ((Pt3 x1 y1 z1), (Pt3 x2 y2 z2)) (Pt3 x y z) = inRange (x1, x2) x && inRange (y1,y2) y && inRange (z1,z2) z
    index :: (Pt3, Pt3) -> Pt3 -> Int
    index = undefined