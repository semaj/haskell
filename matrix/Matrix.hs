module Matrix where
import Data.List

type Matrix = [[Int]]

madd :: Matrix -> Matrix -> Matrix
madd ma mb = zipWith (zipWith (+)) ma mb

isVector :: Matrix -> Bool
isVector m = all (\row -> 1 == length row) m

isRref :: Matrix -> Bool
isRref m =
  where rtc = rowsToColumns m
        a = all (\x -> x == 0 && x == 1) m

mvMult :: Matrix -> Matrix -> Matrix
mvMult a x | not $ isVector x = error "can't multiply two matrices"
mvMult a x = map (dot flat) a
  where flat = foldl (++) [] x

dot :: [Int] -> [Int] -> [Int]
dot x y = zipWith (*) x y

rowsToColumns :: Matrix -> Matrix
rowsToColumns (a:[]) = map (\x -> [x]) a
rowsToColumns (a:b:[]) = zipWith (\x y -> [x, y]) a b
rowsToColumns (a:m) = zipWith (:) a (rowsToColumns m)
