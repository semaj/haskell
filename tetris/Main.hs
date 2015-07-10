import System.IO
import Data.Char
import Matrix

data World = World {
  mine :: String,
  w :: Int,
  h :: Int,
  p :: [[Int]],
  piece :: String,
  pieceL :: (Int, Int),
  nPiece :: String,
  respond :: Bool,
  message :: String
} deriving Show

main :: IO ()
main = run (World "" 0 0 [] "" (0,0) "" False "")

run :: World -> IO ()
run w = do
    input <- getLine
    let next = act $ switch w $ ((words input) :: [String])
    if (respond next) then
      do
        putStrLn (message next)
        hPutStrLn stderr (message next)
        run $ next { message = "", respond = False }
    else
      do
        putStrLn "something"
        hPutStrLn stderr (show $ ((words input) :: [String]))
        hPutStrLn stderr "ahh"
        run next

act :: World -> World
act wo@(World _ w h p piece pieceL _ _ _) = wo { message = "left" }

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


