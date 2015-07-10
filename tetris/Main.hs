import System.IO
import Data.Char

i1 = [[1], [1], [1], [1]]
i2 = [[1,1,1,1]]
is = [i1, i2]

j1 = [[0,1], [0,1], [0,1], [1,1]]
j2 = [[1,1], [1,0], [1,0], [1,0]]
j3 = [[1,1,1,1], [0,0,0,1]]
j4 = [[1,0,0,0], [1,1,1,1]]
js = [j1, j2, j3, j4]

l1 = [[1,0], [1,0], [1,0], [1, 1]]
l2 = [[1,1], [0,1], [0,1], [0,1]]
l3 = [[1,1,1,1], [1,0,0,0]]
l4 = [[0,0,0,1], [1,1,1,1]]
ls = [l1, l2, l3, l4]

os = [ [[1,1],[1,1]] ]

z1 = [[1,1,0], [0,1,1]]
z2 = [[0,1], [1,1], [1,0]]
zs = [z1, z2]

s1 = [[0,1,1], [1,1,0]]
s2 = [[1,0], [1,1], [0,1]]
ss = [s1, s2]

t1 = [[0,1,0], [1,1,1]]
t2 = [[0,1], [1,1], [0,1]]
t3 = [[1,0], [1,1], [1,0]]
t4 = [[1,1,1], [0,1,0]]
ts = [t1, t2, t3 ,t4]

madd :: [[Int]] -> [[Int]] -> [[Int]]
madd x y = zipWith (zipWith (+)) x y

illegal :: [[Int]] -> Bool
illegal x = any (any (== 3)) x

strip :: [[Int]] -> [[Int]]
strip x = filter (all (/= 3)) x

--

data World = World {
  mine :: String,
  w :: Int,
  h :: Int,
  p :: [[Int]],
  piece :: String,
  pieceL :: (Int, Int),
  nPiece :: String,
  respond :: Bool,
  moves :: [String]
} deriving Show

main :: IO ()
main = run (World "" 0 0 [] "" (0,0) "" False [])

run :: World -> IO ()
run w = do
    input <- getLine
    let next = act $ switch w $ ((words input) :: [String])
    if (respond next) then
      do
        putStrLn ((foldl (\a x -> a ++ x ++ ",") "[" (moves next)) ++ "drop]")
        hPutStrLn stderr ((foldl (\a x -> a ++ x ++ ",") "[" (moves next)) ++ "drop]")
        run $ next { moves = [], respond = False }
    else
      do
        hPutStrLn stderr (show $ ((words input) :: [String]))
        hPutStrLn stderr "ahh"
        run next

act :: World -> World
act wo@(World _ w h p piece pieceL _ _ _) = wo { moves = ["left"] }

switch :: World -> [String] -> World
switch w ("settings":typex:value:[]) = settings w typex value
switch w ("action":"moves":time:[]) = action w time
switch w ("update":target:typex:value:[]) = update w target typex value

settings :: World -> String -> String -> World
settings wo "field_width" value = wo { w = ((read value) :: Int) }
settings wo "field_height" value = wo { h = ((read value) :: Int) }
settings wo "your_bot" value = wo { mine = value }
settings wo x y = wo

action :: World -> String -> World
action wo value = wo { respond = True }

update :: World -> String -> String -> String -> World
update wo "game" "this_piece_type" value = wo { piece = value }
update wo "game" "next_piece_type" value = wo { nPiece = value }
update wo "game" "this_piece_position" (x:',':y:[]) = wo { pieceL = (ord x, ord y) }
update wo player "field" value = if player == (mine wo) then wo { p = convert value } else wo
update wo player target value = wo

convert :: String -> [[Int]]
convert s = map (\l -> map read $ l :: [Int]) $ map (splitBy ',') (splitBy ';' s)


splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                               | otherwise = (c:x):xs


