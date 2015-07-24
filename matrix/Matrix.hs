module Matrix where
import Data.List

type Matrix = [[Int]]

madd :: Matrix -> Matrix -> Matrix
madd ma mb = zipWith (zipWith (+)) ma mb

isVector :: Matrix -> Bool
isVector m = all ((== 1) . length) m

-- isRref :: Matrix -> Bool
-- isRref m = a &&
--   where rtc = rowsToColumns m
--         findNonZero = map (find (/= 0)) rtc
--         a = all (all (\x -> x == Maybe 1 || Nothing)) findNonZero
--         oneCount = countTrue (== 1) m
--         b = oneCount == 1 || oneCount == 0
--         c =

countTrue :: (a -> Bool) -> [a] -> Int
countTrue f x = length $ filter f x

mvMult :: Matrix -> Matrix -> Matrix
mvMult a x | not $ isVector x = error "can't multiply two matrices"
mvMult a x = map (dot $ concat x) a

dot :: [Int] -> [Int] -> [Int]
dot x y = zipWith (*) x y

rowsToColumns :: Matrix -> Matrix
rowsToColumns (a:[]) = map (\x -> [x]) a
rowsToColumns (a:m) = zipWith (:) a (rowsToColumns m)
