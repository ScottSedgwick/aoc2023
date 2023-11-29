module Search (bfs, bfs2, searchOrder, binSearch) where

import qualified Data.Set as S
import Data.Sequence ((><), Seq(..), fromList)

bfs :: (state -> [state]) -> state -> [state]
bfs step state = if null states then [state] else concatMap (\x -> bfs step x) states 
  where
    states = step state

bfs2 ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]        {- ^ initial states            -} ->
  [a]        {- ^ reachable states          -}
bfs2 rep next xs = loop S.empty (fromList xs)
  where
    loop _     Empty      = []
    loop !seen (q :<| qs) =  
        if S.member r seen
        then loop seen qs
        else q : loop seen' q'
      where
        r     = rep q
        seen' = S.insert r seen
        q'    = qs >< (fromList (next q))

data SearchOrder = SoUp | SoDown deriving stock (Show, Eq)

searchOrder :: Ord b => (a -> b) -> (a, a) -> SearchOrder
searchOrder f (lo,hi) = if f lo < f hi then SoUp else SoDown
  
binSearch :: Int -> (Int -> Int) -> (Integer, Integer) -> SearchOrder -> Maybe Integer
binSearch val f (lo, hi) so = 
    if lo == hi 
        then Nothing
        else case compare (f (fromIntegral mid)) val of
                GT -> if so == SoUp then binSearch val f (lo, mid) so else binSearch val f (mid, hi) so
                LT -> if so == SoUp then binSearch val f (mid, hi) so else binSearch val f (lo, mid) so
                EQ -> Just mid
  where
    mid = (lo + hi) `div` 2