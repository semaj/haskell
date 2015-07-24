
divides :: Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

-- Exercise 1.4 
-- Suppose in the definition of ldf we replace the condition k^2 > n by k^2 >= n, where >= expresses ‘greater than or equal’. Would that make any difference to the meaning of the program? Why (not)?
-- =>> No. If k^2 == n, then k can be divided into n, which would have been caught in the previous condition.

prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not positive integer"
         | n == 1 = False
         | otherwise = ld n == n
