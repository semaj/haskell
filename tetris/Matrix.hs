module Matrix where
import Data.List

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

