import System.IO

-- Day 5: Binary Boarding
-- What is the highest seat ID on a boarding pass?

maxId :: [String] -> Int
maxId ls = maximum ( map (\x -> seatHelper x 0 127) ls )

seatHelper :: [Char] -> Int -> Int -> Int
seatHelper [] ll ul = ul
seatHelper ('F':xs) ll ul = seatHelper xs ll (div (ul + ll) 2)
seatHelper ('B':xs) ll ul = seatHelper xs ((div (ul + ll) 2) + 1) ul
seatHelper xs ll ul = ul * 8 + column xs 0 7

column :: [Char] -> Int -> Int -> Int
column [] ll ul = ul
column ('R':xs) ll ul = column xs ((div (ul + ll) 2) + 1) ul
column ('L':xs) ll ul = column xs ll (div (ul + ll) 2)


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main ::  IO()
main = do
  filecontent <- readLines "day5.txt"
  print (maxId filecontent)
