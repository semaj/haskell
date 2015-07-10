import System.IO

data World = World {
  mine :: String,
  w :: Int,
  h :: Int,
  p1 :: [[Int]],
  message :: String
}

main :: IO ()
main = run (World "" 0 0 [] "")

run :: World -> IO ()
run w = do
    input <- getLine
    let next = switch w $ ((words input) :: [String])
    if (message w) /= "" then
      do
        putStrLn (message w)
        run next
    else
      run next


switch :: World -> [String] -> World
switch w ("settings":typex:value:[]) = settings w typex value
switch w ("action":"moves":time:[]) = action w time
switch w ("update":target:typex:value:[]) = update w target typex value

settings :: World -> String -> String -> World
settings w typex value = w

action :: World -> String -> World
action w value = w

update :: World -> String -> String -> String -> World
update w target typex value = w
