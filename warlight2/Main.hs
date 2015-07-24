import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Data.List

data SuperRegion = SuperRegion {
  srId :: Int,
  bonus :: Int
} deriving Show

data Region = Region {
  rId :: Int,
  super :: SuperRegion,
  owner :: String,
  armies :: Int,
  neighbors :: [Region]
} deriving Show

data PickInfo = PickInfo {
  startingRegions :: [Region],
  startingArmies :: Int,
  startingPickAmount :: Int
} deriving Show

data World = World {
  regions :: [Region],
  superRegions :: [SuperRegion],
  wastelands :: [Region],
  myBot :: String,
  otherBot :: String,
  pickInfo :: PickInfo,
  action :: String
} deriving Show

main :: IO ()
main = run $ World [] [] [] "" "" (PickInfo [] 0 0) "someaction"

run :: World -> IO ()
run world = do
    isOpen <- hIsOpen stdin
    unless isOpen main
    input <- getLine
    let nextWorld = step (words input) world
    putStrLn (action nextWorld)
    --hPutStrLn stderr nextWorld
    run nextWorld

groupIn :: Int -> [a] -> [[a]]
groupIn _ [] = []
groupIn n l
  | n > 0 = (take n l) : (groupIn n (drop n l))
  | otherwise = error "Negative n"

parseIList :: [String] -> [[String]]
parseIList s = groupIn 2 $ words $ filter (/= '[') $ foldl (++) "" s

getSuperRegions :: [String] -> [SuperRegion]
getSuperRegions s = map (\ (id:bonus:[]) -> (SuperRegion (read id :: Int) (read bonus :: Int))) $
                        parseIList s

superById :: Int -> [SuperRegion] -> SuperRegion
superById id supers = head $ filter ((== id) . (srId)) supers

getRegions :: [String] -> [SuperRegion] -> [Region]
getRegions s supers = map (\ (id:super:[]) -> (Region (read id :: Int) (superById (read super :: Int) supers) "" 0 [])) $
                          parseIList s

step :: [String] -> World -> World
step ("setup_map":"super_regions":regions) world = world
step ("setup_map":"regions":regions) world = world { regions = (getRegions regions (superRegions world)) }
step ("setup_map":"neighbors":neighbors) world = world
step input world = world

