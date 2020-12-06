import System.IO
import Data.List

-- Day 5: Binary Boarding
-- What is the highest seat ID on a boarding pass?

seatIds :: [String] -> [Int]
seatIds ls = map (\x -> seatHelper x 0 127) ls

seatHelper :: [Char] -> Int -> Int -> Int
seatHelper [] ll ul = ul
seatHelper ('F':xs) ll ul = seatHelper xs ll (div (ul + ll) 2)
seatHelper ('B':xs) ll ul = seatHelper xs ((div (ul + ll) 2) + 1) ul
seatHelper xs ll ul = ul * 8 + column xs 0 7

column :: [Char] -> Int -> Int -> Int
column [] ll ul = ul
column ('R':xs) ll ul = column xs ((div (ul + ll) 2) + 1) ul
column ('L':xs) ll ul = column xs ll (div (ul + ll) 2)

-- Part 2: What is the ID of your seat?
missingId :: [Int] -> Int
missingId [] = 0
missingId (x:y:xs) = if abs(x-y) == 1
                     then missingId (y:xs)
                     else div (x+y) 2


-- Read file contents
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main ::  IO()
main = do
  filecontent <- readLines "day5.txt"
  print (maximum (seatIds filecontent))
  let sortedIds = sort (seatIds filecontent)
  print (missingId sortedIds)

